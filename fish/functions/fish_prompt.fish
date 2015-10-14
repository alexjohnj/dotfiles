# name: ajl
# Heavily inspired by Jorge Israel Peña's prompt:
# http://www.blaenkdenum.com/posts/terminal-customization/#prompt

function _git_branch_name
  echo (command git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
end

function _git_is_clean --description "Returns 0 if clean, 1 otherwise"
  _git_has_untracked_files
  set -l has_untracked_files $status

  # Check for stages and unstaged changes
  not git diff-index --quiet HEAD
  set -l has_changes $status

  test \( $has_untracked_files -eq 1 \) -a \( $has_changes -eq 1 \)
  return $status
end

function _git_has_untracked_files --description "Returns 0 if there are untracked files, 1 otherwise"
  not test -z (git ls-files --exclude-standard --others ^/dev/null)
  return $status
end

function _git_has_changes --description "Returns 0 if there are changes, 1 otherwise"
  not git diff-files --quiet ^/dev/null
  return $status
end

function _git_has_staged_changes --description "Returns 0 if there are staged changes, 1 otherwise"
  not git diff-index --quiet --cached HEAD ^/dev/null
  return $status
end

function _make_prompt_segment
  set -l bg $argv[1]
  set -l fg $argv[2]

  set_color -b $bg
  set_color $fg

  if [ -n "$argv[3]" ]
    echo -n -s $argv[3]
  end
end

function _print_lambda
  if [ $last_status -eq 0 ]
    _make_prompt_segment normal blue "λ"
  else
    _make_prompt_segment normal red "λ"
  end
end

function _print_cwd
  _make_prompt_segment normal normal (prompt_pwd)
end

function _print_git_status --description "Returns 0 if the status was printed, 1 otherwise"
  set -l git_branch (_git_branch_name)

  if not [ $git_branch ]
    return 1
  end

  set git_info $git_branch

  if  _git_is_clean
    _make_prompt_segment green white " $git_info "
    return
  end

  if _git_has_untracked_files
    set git_info "$git_info ⦁"
  end

  if _git_has_changes
    set git_info "$git_info ▲"
  end

  if _git_has_staged_changes
    set git_info "$git_info ✚"
  end

  _make_prompt_segment red white " $git_info "
  return 0
end

function _print_arrow
  _make_prompt_segment normal blue "❯"
end

function _print_spacing
  _make_prompt_segment normal normal " "
end

function fish_prompt
  set -g last_status $status
  
  _print_lambda
  _print_spacing
  _print_cwd
  _print_spacing
  _print_git_status
  # Avoid double spacing between cwd and git status
  if test $status -eq 0
    _print_spacing
  end
  _print_arrow
  _print_spacing
end
