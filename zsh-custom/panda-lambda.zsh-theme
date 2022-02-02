local cyan="%{$fg[cyan]%}"
local red="%{$fg[red]%}"
local yellow="%{$fg[yellow]%}"
local bold_blue="%{$fg_bold[blue]%}"
local bold_green="%{$fg_bold[green]%}"
local bold_red="%{$fg_bold[red]%}"
local reset_color="%{$reset_color%}"
local working_dir="%c"
local lambda="%(?.${bold_green}λ .${bold_red}λ )"

ZSH_THEME_GIT_PROMPT_PREFIX="${bold_blue}(${red}"
ZSH_THEME_GIT_PROMPT_SUFFIX="${reset_color} "
ZSH_THEME_GIT_PROMPT_DIRTY="${yellow}*${bold_blue})"
ZSH_THEME_GIT_PROMPT_CLEAN="${bold_blue})"

PROMPT='${cyan}${working_dir}${reset_color} $(git_prompt_info)${lambda}${reset_color}'
