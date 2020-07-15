sed -i'' \
    `# comment 1` \
    -e 's/search 1/replace 1/g' \
    `: # comment 2` \
    -e 's/search 2/replace 2/g' \
    "input.txt"