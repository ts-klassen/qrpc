def normalize_styles($styles):
  ($styles | map({
      name: .name,
      id: .id,
      type: ((.type // "talk") | if . == "singing_teacher" then "sing" else . end)
    }));

def add_styles($entry; $styles):
  reduce $styles[] as $style ($entry;
    if (.styles | map({id, type}) | index({id: $style.id, type: $style.type})) then .
    else
      .styles += [$style]
      | if ($style.type == "frame_decode" or $style.type == "sing") then .isSinger = true else . end
    end
  );

def metas_in_order:
  to_entries
  | map(. as $entry | ($entry.value.metas // []) | map(. + {vvm_key: $entry.key}))
  | add // [];

(metas_in_order) as $metas
| reduce $metas[] as $meta ({speakers: {}, order: []};
    ($meta.speaker_uuid // "") as $uuid
    | ($meta.vvm_key // "") as $vvm_key
    | ($vvm_key | startswith("n")) as $is_nemo
    | if $uuid == "" then .
      else
        if .speakers[$uuid] == null then
          .order += [$uuid]
          | .speakers[$uuid] = {
              prefix: "VOICEVOX:",
              isNemo: $is_nemo,
              name: $meta.name,
              styles: [],
              terms: "",
              isSinger: false
            }
        else . end
        | .speakers[$uuid] = add_styles(.speakers[$uuid]; normalize_styles($meta.styles // []))
        | if $is_nemo then .speakers[$uuid].isNemo = true else . end
      end
  )
| reduce (($name_overrides[0] // {}) | keys[]) as $uuid (. ;
    if .speakers[$uuid] then .speakers[$uuid].name = $name_overrides[0][$uuid] else . end
  )
| reduce (($terms_overrides[0] // {}) | keys[]) as $uuid (. ;
    if .speakers[$uuid] then .speakers[$uuid].terms = $terms_overrides[0][$uuid] else . end
  )
| .order as $order
| reduce $order[] as $uuid ({order: [], seen: {}};
    if .seen[$uuid] then .
    else .order += [$uuid] | .seen[$uuid] = true end
  ) as $deduped
| . as $state
| ($order_overrides[0] // []) as $overrides
| ($overrides | map(select($state.speakers[.]))) as $front
| .order = ($front + ($deduped.order | map(. as $uuid | select(($overrides | index($uuid)) | not))))
| . as $state
| ($state.order | map({key: ., value: $state.speakers[.]}))
| from_entries
