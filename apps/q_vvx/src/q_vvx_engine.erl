-module(q_vvx_engine).

-include_lib("qrpc/include/qrpc.hrl").

-export([
        super_simple_tts/1
    ]).

super_simple_tts(Rpc) ->
    Base = case ?QRPC_SUBCONF_LOOKUP(workers) of
        {value, [{voicevox_http_engine, VHE}|_]} ->
            VHE;
        OptnlWorkers ->
            ?QRPC_ERROR(#{
                id => [q_vvx, engine, super_simple_tts, bad_config]
              , fault_source => server
              , message => <<"No good simple tts engine config">>
              , message_ja => <<"良いシンプルなTTSエンジンのコンフィグがありません"/utf8>>
              , detail => #{
                    optnl_workers => OptnlWorkers
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end,
    Speaker = Rpc:get([payload, <<"id">>]),
    case Speaker of
        3 -> ok;
        _ ->
            ?QRPC_ERROR(#{
                id => [q_vvx, engine, super_simple_tts, unsupported_speaker]
              , fault_source => client
              , message => <<"Unsupported speaker">>
              , message_ja => <<"未対応の話者です"/utf8>>
              , detail => #{
                    speaker => Speaker
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end,
    SpeakerBin = klsn_binstr:from_any(Speaker),
    Text = Rpc:get([payload, <<"text">>]),
    case Text of
        Text0 when is_binary(Text0) andalso size(Text0) < 1500 -> ok;
        _ ->
            ?QRPC_ERROR(#{
                id => [q_vvx, engine, super_simple_tts, unsupported_text]
              , fault_source => client
              , message => <<"Unsupported text">>
              , message_ja => <<"未対応の文字列です"/utf8>>
              , detail => #{
                    text => Text
                }
              , is_known => true
              , is_retryable => false
              , version => 1
            })
    end,
    case qrpc_counter:add({?MODULE, super_simple_tts, Base}, {slot, minutely}, size(Text)) of
        Count when Count < 10000 -> ok;
        Count ->
            Exp = qrpc_counter:parse_exp({slot, minutely}),
            ?QRPC_ERROR(#{
                id => [q_vvx, engine, super_simple_tts, minutely_limit_exceeded]
              , fault_source => server
              , message => <<"minutely limit exceeded on server side">>
              , message_ja => <<"サーバー全体が 1 分あたりの制限に達しました"/utf8>>
              , detail => #{
                    exp => Exp
                  , count => Count
                }
              , is_known => true
              , is_retryable => true
              , should_auto_retry => true
              , retry_after => Exp - erlang:system_time(second) + 1
              , version => 1
            })
    end,
    AudioQueryUrl = <<
        Base/binary
      , "/audio_query?speaker="
      , SpeakerBin/binary
      , "&text="
      , (klsn_binstr:urlencode(Text))/binary
    >>,
    AudioQueryRes = httpc:request(post, {AudioQueryUrl, [], "application/json", []}, [], [{body_format, binary}]),
    AudioQuery = case AudioQueryRes of
        {ok, {{_, 200, _}, _, AudioQuery0}} ->
            json:encode(#{
                accent_phrases => maps:get(<<"accent_phrases">>, json:decode(AudioQuery0))
              , speedScale => 1
              , pitchScale => 0
              , intonationScale => 1
              , volumeScale => 2
              , prePhonemeLength => 0.1
              , postPhonemeLength => 0.1
              , outputSamplingRate => 24000
              , outputStereo => false
            });
        _ ->
            ?QRPC_ERROR(#{
                id => [q_vvx, engine, super_simple_tts, audio_query_failed]
              , fault_source => external
              , message => <<"Call for audio_query failed">>
              , message_ja => <<"audio_query の呼び出しに失敗しました"/utf8>>
              , detail => #{
                    httpc_res => AudioQueryRes
                }
              , is_known => false
              , is_retryable => false
              , version => 1
            })
    end,
    AudioRes = httpc:request(post, {<<Base/binary, "/synthesis?speaker=", SpeakerBin/binary>>, [], "application/json", AudioQuery}, [], [{body_format, binary}]),
    WAV = case AudioRes of
        {ok, {{_, 200, _}, _, WAV0}} ->
            WAV0;
        _ ->
            ?QRPC_ERROR(#{
                id => [q_vvx, engine, super_simple_tts, synthesis_failed]
              , fault_source => external
              , message => <<"Call for synthesis failed">>
              , message_ja => <<"synthesis の呼び出しに失敗しました"/utf8>>
              , detail => #{
                    httpc_res => AudioRes
                }
              , is_known => false
              , is_retryable => false
              , version => 1
            })
    end,
    #{
        payload => #{}
      , blob => [#{
            content_type => <<"audio/wav">>
          , data => WAV
        }]
    }.
