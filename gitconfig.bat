REM ensure the current commit hash is added to *.dspec
REM Only needed for package publishing - for owner use only.

git config filter.insert-hash.smudge "sh -c 'hash=$(git rev-parse HEAD); sed s/#HASH#/Id:$hash/g'"
git config filter.insert-hash.clean "sh -c 'sed -E 's/Id:[0-9a-fA-F]{40}/#HASH#/g''"
copy post-commit.txt .git\hooks\post-commit