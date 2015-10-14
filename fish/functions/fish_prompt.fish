# name: ajl
# Heavily inspired by Jorge Israel Peña's prompt:
# http://www.blaenkdenum.com/posts/terminal-customization/#prompt

function _git_branch_name
  echo (command git symbolic-ref HEAD ^/dev/null | sed -e 's|^refs/heads/||')
end

function _is_git_dirty
  echo (command git status -s --ignore-submodules=dirty ^/dev/null)
end

function _git_untracked_count
  echo (command git status -s ^/dev/null | grep "??" ^/dev/null | wc -l | tr -d ' ')
end

function _git_changed_count
  echo (command git diff --name-only HEAD ^/dev/null | wc -l | tr -d ' ')
end

function _git_staged_count
  echo (command git diff --cached --numstat ^/dev/null | wc -l | tr -d ' ')
end

function fish_prompt
  set -l last_stat $status
  set -l red (set_color red)
  set -l blue (set_color blue)
  set -l green (set_color green)
  set -l normal (set_color normal)
  set -l bggreen (set_color --bold --background green white)
  set -l bgred (set_color --bold --background red white)

  set -l lambda
  if [ $last_stat -ne 0 ]
    set lambda $red"λ"
  else
    set lambda $blue"λ"
  end
  
  set -l arrow $blue"❯❯"
  set -l cwd $normal(prompt_pwd)

  set -l git_info
  if [ (_git_branch_name) ]
    set -l git_changed_count (_git_changed_count)
    set -l git_staged_count (_git_staged_count)
    set -l git_untracked_count (_git_untracked_count)
    
    set git_info (_git_branch_name)

    # Get the right spacing after the branch name
    if [ $git_untracked_count -gt 0 ]
      set git_info "$git_info ⦁"
    end

    if [ $git_changed_count -gt 0 ]
      set git_info "$git_info ▲"
    end

    if [ $git_staged_count -gt 0 ]
      set git_info "$git_info ✚"
    end
    
    if [ (_is_git_dirty) ]
      set git_info $bgred"["$git_info"]$normal "
    else
      set git_info $bggreen"["$git_info"]$normal "
    end
  else
    set git_info ""
  end

  echo -n -s $lambda ' ' $cwd ' ' $git_info $arrow ' ' $normal
end
