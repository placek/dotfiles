function __done_get_focused_window_id
  if type -q lsappinfo
    lsappinfo info -only bundleID (lsappinfo front) | cut -d '"' -f4
  else if test -n "$SWAYSOCK"
    and type -q jq
    swaymsg --type get_tree | jq '.. | objects | select(.focused == true) | .id'
  else if begin
      test "$XDG_SESSION_DESKTOP" = gnome; and type -q gdbus
    end
    gdbus call --session --dest org.gnome.Shell --object-path /org/gnome/Shell --method org.gnome.Shell.Eval 'global.display.focus_window.get_id()'
  else if type -q xprop
    and test -n "$DISPLAY"
    and xprop -grammar >/dev/null 2>&1
    xprop -root 32x '\t$0' _NET_ACTIVE_WINDOW | cut -f 2
  end
end

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

# verify that the system has graphical capabilities before initializing
if test -z "$SSH_CLIENT"
  and count (__done_get_focused_window_id) >/dev/null
  set __done_enabled
end

if set -q __done_enabled
  set -g __done_initial_window_id ''
  set -q __done_min_cmd_duration; or set -g __done_min_cmd_duration 5000
  set -q __done_sway_ignore_visible; or set -g __done_sway_ignore_visible 0

  function __done_started --on-event fish_preexec
    set __done_initial_window_id (__done_get_focused_window_id)
  end

  function __done_ended --on-event fish_prompt
    set -l exit_status $status

    # backwards compatibility for fish < v3.0
    set -q cmd_duration; or set -l cmd_duration $CMD_DURATION

    if test $cmd_duration
      and test $cmd_duration -gt $__done_min_cmd_duration

      set -l humanized_duration (__done_humanize_duration "$cmd_duration")
      set -l title "Done in $humanized_duration"
      set -l wd (string replace --regex "^$HOME" "~" (pwd))
      set -l message "$wd/ $history[1]"
      set -l sender $__done_initial_window_id

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
