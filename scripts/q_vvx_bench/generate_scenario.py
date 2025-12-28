#!/usr/bin/env python3
import argparse
import json
import random


STYLE_DIST = {0: 0.15, 1: 0.20, 2: 0.25, 3: 0.20, 4: 0.12, 5: 0.08}
CHAR_DIST = {
    "の": 0.09262,
    "に": 0.05354,
    "た": 0.05155,
    "い": 0.05119,
    "は": 0.04528,
    "を": 0.04521,
    "と": 0.04480,
    "る": 0.04425,
    "が": 0.04156,
    "し": 0.04095,
    "で": 0.03693,
    "て": 0.03661,
    "な": 0.03477,
    "か": 0.02594,
    "っ": 0.02257,
    "れ": 0.02177,
    "ら": 0.02044,
    "も": 0.01913,
    "う": 0.01704,
    "す": 0.01645,
    "り": 0.01613,
    "こ": 0.01508,
    "だ": 0.01356,
    "ま": 0.01345,
    "さ": 0.01250,
    "き": 0.01127,
    "め": 0.01081,
    "く": 0.01072,
    "あ": 0.00986,
    "け": 0.00963,
    "ど": 0.00949,
    "ん": 0.00918,
    "え": 0.00790,
    "よ": 0.00745,
    "つ": 0.00744,
    "や": 0.00706,
    "そ": 0.00635,
    "わ": 0.00594,
    "ち": 0.00479,
    "み": 0.00431,
    "せ": 0.00403,
    "ろ": 0.00355,
    "ば": 0.00349,
    "お": 0.00318,
    "じ": 0.00275,
    "べ": 0.00270,
    "ず": 0.00257,
    "げ": 0.00237,
    "ほ": 0.00235,
    "へ": 0.00227,
}
LENGTH_MEAN = 34.549915701115616
LENGTH_STDDEV = 27.648471063654874
LENGTH_MIN = 1
LENGTH_MAX = 285


def pick_weighted(dist):
    items = list(dist.items())
    values, weights = zip(*items)
    return random.choices(values, weights=weights, k=1)[0]


def clamp(value, lo, hi):
    return max(lo, min(hi, value))


def pick_length():
    return int(round(clamp(random.gauss(LENGTH_MEAN, LENGTH_STDDEV), LENGTH_MIN, LENGTH_MAX)))


def load_style_dist(path):
    with open(path, "r", encoding="utf-8") as f:
        data = json.load(f)
    return {int(key): value for key, value in data.items()}


def build_scenario(total, total_time, style_dist, char_dist):
    mean = total_time / 2
    stddev = total_time / 6
    scenario = []
    for _ in range(total):
        style_id = pick_weighted(style_dist)
        text_len = pick_length()
        text = "".join(pick_weighted(char_dist) for _ in range(text_len))
        offset = int(clamp(random.gauss(mean, stddev), 0, total_time))
        scenario.append(
            {
                "offset_ms": offset,
                "style_id": style_id,
                "text": text,
            }
        )
    scenario.sort(key=lambda item: item["offset_ms"])
    return scenario


def main():
    parser = argparse.ArgumentParser(
        description="Generate a dummy q_vvx_bench scenario.json with Gaussian offsets."
    )
    parser.add_argument("--out", default="scenario.json")
    parser.add_argument("--total", type=int, default=100)
    parser.add_argument("--total-time", type=int, default=100000)
    parser.add_argument("--seed", type=int, default=42)
    parser.add_argument(
        "--style-dist",
        default=None,
        help="Path to a JSON object of {style_id: weight}.",
    )
    args = parser.parse_args()

    random.seed(args.seed)
    style_dist = STYLE_DIST
    if args.style_dist:
        style_dist = load_style_dist(args.style_dist)
    scenario = build_scenario(args.total, args.total_time, style_dist, CHAR_DIST)

    with open(args.out, "w", encoding="utf-8") as f:
        json.dump(scenario, f, ensure_ascii=False, indent=2)


if __name__ == "__main__":
    main()
