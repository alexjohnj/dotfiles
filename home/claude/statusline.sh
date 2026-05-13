input=$(cat)

IFS=$'\t' read -r model ctx_used ctx_size five_hour seven_day cost current_dir <<<"$(
  jq -r '[
    .model.display_name,
    (.context_window.total_input_tokens // ""),
    (.context_window.context_window_size // ""),
    (.rate_limits.five_hour.used_percentage // ""),
    (.rate_limits.seven_day.used_percentage // ""),
    (.cost.total_cost_usd // ""),
    .workspace.current_dir
  ] | @tsv' <<<"$input"
)"

branch=$(git -C "$current_dir" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null || echo "")

fmt_tokens() {
  n=$1
  if [ "$n" -ge 1000000 ]; then
    printf "%dM" "$((n / 1000000))"
  elif [ "$n" -ge 1000 ]; then
    printf "%dK" "$((n / 1000))"
  else
    printf "%d" "$n"
  fi
}

parts="$model"
[ -n "$branch" ] && parts="$parts | $branch"
if [ -n "$ctx_used" ] && [ -n "$ctx_size" ]; then
  parts="$parts | ctx: $(fmt_tokens "$ctx_used")/$(fmt_tokens "$ctx_size")"
fi
[ -n "$five_hour" ] && parts="$parts | 5h: $(printf '%.0f' "$five_hour")%"
[ -n "$seven_day" ] && parts="$parts | 7d: $(printf '%.0f' "$seven_day")%"
[ -n "$cost" ] && parts="$parts | \$$(printf '%.4f' "$cost")"

echo "$parts"
