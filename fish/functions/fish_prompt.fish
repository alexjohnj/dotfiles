# name: ajl

function _git_branch_name
  echo (command git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
end

function _git_is_clean --description "Returns 0 if clean, 1 otherwise"
  _git_has_untracked_files
  set -l has_untracked_files $status

  # Check for stages and unstaged changes
  not git diff-index --quiet HEAD
  set -l has_changes $status

  [ \( $has_untracked_files -eq 1 \) -a \( $has_changes -eq 1 \) ]
  return $status
end

function _get_git_origin_state
# Echoes 'ok' if up to date, 'pull' if need to pull or 'push' if need
# to push. From here:
# http://stackoverflow.com/questions/3258243/git-check-if-pull-needed
  set -l local (git rev-parse @\{0\})
  set -l remote (git rev-parse @\{u\})
  set -l base (git merge-base @ @\{u\})

  if [ $local = $remote ]
    echo "ok"
  else if [ $local = $base ]
    echo "pull"
  else if [ $remote = $base ]
    echo "push"
  end
end

function _git_has_untracked_files --description "Returns 0 if there are untracked files, 1 otherwise"
  set -l git_ls_output (echo (git ls-files --exclude-standard --others))
  not [ -z $git_ls_output ]
  return $status
end

function _is_ssh_session --description "Returns 0 if currently in SSH session, 1 otherwise"
  [ -n "$SSH_CLIENT" ]
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

function _print_ssh -d "Returns 0 if SSH was printed, 1 otherwise"
  if _is_ssh_session 
    _make_prompt_segment normal normal $USER
    _make_prompt_segment normal blue "@"
    _make_prompt_segment normal normal (hostname -s)
    return 0
  end
  return 1
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

  if not _git_is_clean
    _make_prompt_segment normal red $git_info
    return 0
  end

  set -l origin_state (_get_git_origin_state)
  if [ $origin_state = "ok" ]
    _make_prompt_segment normal green $git_info
  else if [ $origin_state = "pull" ]
    _make_prompt_segment normal blue $git_info
  else if [ $origin_state = "push" ]
    _make_prompt_segment normal yellow $git_info
  end
  return 0
end

function _print_arrow
  if [ $last_status -eq 0 ]
    _make_prompt_segment normal green "\$"
  else
    _make_prompt_segment normal red "\$"
  end
end

function _print_spacing
  _make_prompt_segment normal normal " "
end

function fish_prompt
  set -g last_status $status
  
  _print_ssh
  if [ $status -eq 0 ]
    _print_spacing
  end
  _print_arrow
  _print_spacing
end

function fish_right_prompt
  _make_prompt_segment normal normal "["
  _print_cwd

  if [ -n (_git_branch_name) ]
    _make_prompt_segment normal normal ":"
  end
  _print_git_status
  _make_prompt_segment normal normal "]"
end
