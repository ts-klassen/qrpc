# q_vvx_bench scenario generator

Generate `scenario.json` for `q_vvx_bench` using:
- style_id distribution from `speaker_dist.json`
- character distribution from Japanese hiragana frequency (top 50)
- text length from a Gaussian distribution (mean/stddev derived from dataset stats)

Usage (from this directory):
```
python3 generate_scenario.py --out scenario.json \
  --style-dist speaker_dist_talk.json \
  --total 100 --total-time 100000 --seed 42
```

Options:
- `--out`: output path (default: `scenario.json`)
- `--style-dist`: path to a JSON object of `{style_id: weight}`
- `--total`: number of items to generate
- `--total-time`: upper bound for `offset_ms` (milliseconds)
- `--seed`: RNG seed for reproducibility

Notes:
- `offset_ms` is drawn from `N(total_time/2, (total_time/6)^2)` and clamped to `[0, total_time]`.
- `text` length is drawn from `N(34.5499, 27.6485^2)` and clamped to `[1, 285]`.
- Weights do not need to sum to 1; they are treated as relative weights.

Erlang example:
```
q_vvx_bench:synthesize(#{
    scenario => "scripts/q_vvx_bench/scenario.json",
    result => "scripts/q_vvx_bench/result.json"
}).
```
