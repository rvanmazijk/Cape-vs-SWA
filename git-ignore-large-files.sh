find . -not -path *.git* -type f -size +100M |
  sed 's|^\./||g' |
  cat >> .gitignore
