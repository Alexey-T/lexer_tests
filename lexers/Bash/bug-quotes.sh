TDATA=()
DLCS=()
for i in "${OUTPUT}/DLCs/"*/Readme.txt; do
  while read -r tdata; do
    if [[ $tdata =~ 'content stored' ]]; then
      EPATH="${tdata:(-16):(-2)}"
      TITLEID="${EPATH##*\\}"
      TITLEID="${TITLEID^^}"
      TDATA+=( ${OUTPUT}/${EPATH} )
      XPATH="${i%/*}"
      XPATH="D:\DLCs\\${XPATH##*/}"
      DIR="${TITLENAME[$TITLEID]:-}"
      [[ -z $DIR ]] && DIR="${XPATH##*\\}"
      DLCS+=( "<List Text=\"${DIR}\" Icon=\"${XPATH}\Icon.jpg\" Sort=\"Off\" Batch=\"True\">" )
      DLCS+=( "  <Item Action=\"Unzip\" Arg1=\"D:\\TDATA\\${TITLEID}.zip\" Arg2=\"E:\\\" />" )
      DLCS+=( "  <Item Action=\"${XPATH}\default.xbe\" />" )
      DLCS+=( '</List>' )
      #add_artworks "$TITLEID" "${i%/*}"
    fi
  done < "$i"
done