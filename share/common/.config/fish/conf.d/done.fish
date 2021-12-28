function __done_humanize_duration -a milliseconds
  set -l seconds (math --scale=0 "$milliseconds/1000" % 60)
  set -l minutes (math --scale=0 "$milliseconds/60000" % 60)
  set -l hours (math --scale=0 "$milliseconds/3600000")

  if test $hours -gt 0
    printf '%s' $hours'h '
  end
  if test $minutes -gt 0
    printf '%s' $minutes'm '
  end
  if test $seconds -gt 0
    printf '%s' $seconds's'
  end
end

if set -q __done_enabled
  set -q __done_min_cmd_duration; or set -g __done_min_cmd_duration 5000

  function __done_ended --on-event fish_prompt
    set -l exit_status $status

    if test $cmd_duration
      and test $cmd_duration -gt $__done_min_cmd_duration

      set -l humanized_duration (__done_humanize_duration "$cmd_duration")
      set -l title "Done in $humanized_duration"
      set -l wd (string replace --regex "^$HOME" "~" (pwd))
      set -l message "$wd/ $history[1]"

      if test $exit_status -ne 0
        set title "Failed ($exit_status) after $humanized_duration"
      end

      if type -q notify-send # notify-send
        set -l urgency low

        if test $exit_status -ne 0
          set urgency critical
        end

        notify-send --urgency=$urgency --icon=utilities-terminal --app-name=fish "$title" "$message"

      else # anything else
        echo -e "\a"
      end
    end
  end
end
