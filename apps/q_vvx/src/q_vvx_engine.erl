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
    AudioQueryUrl = <<
        Base/binary
      , "?/audio_query?speaker="
      , (klsn_binstr:from_any(Speaker))/binary
      , "&text="
      , (klsn_binstr:urlencode(Text))/binary
    >>,
    AudioQueryRes = httpc:request(get, {AudioQueryUrl, []}, [], [{body_format, binary}]),
    AudioQuery = case AudioQueryRes of
        {ok, {{_, 200, _}, _, AudioQuery0}} ->
            AudioQuery0;
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
    AudioRes = httpc:request(post, {<<Base/binary, "/synthesis">>, [], "application/json", AudioQuery}, [], [{body_format, binary}]),
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
        base64_wav_audio => base64:encode(WAV)
    }.

