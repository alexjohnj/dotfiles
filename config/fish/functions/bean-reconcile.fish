#!/usr/bin/env fish

function bean-reconcile
  set TARGET_ACCOUNT ""
  if [ (count $argv) -eq 0 ]
    set TARGET_ACCOUNT "Assets:"
  else
    set TARGET_ACCOUNT $argv[1]
  end

  printf "Cleared Totals:\n"
  bean-query $BEANCOUNT_FILE 'SELECT account, sum(position) WHERE account ~ ' "\"$TARGET_ACCOUNT\"" ' AND flag != "!" GROUP BY account'

  printf "\n\nUncleared Totals:\n"
  bean-query $BEANCOUNT_FILE 'SELECT account, sum(position) WHERE account ~ ' "\"$TARGET_ACCOUNT\"" ' GROUP BY account'

  printf "\n\nPending Transactions:\n"
  bean-query $BEANCOUNT_FILE 'SELECT date, account, payee, position WHERE flag = "!" AND account ~ ' "\"$TARGET_ACCOUNT\"" ' ORDER BY account, date DESC'
end
