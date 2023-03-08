Function CCD_info_read, filename = fnam, head = head, fullname = wfnam, $
	_extra = _e

	on_error, 1
	blkn = 5

	wfnam = File_get(fnam,stat=stat,_extra=_e)
	if stat then begin
		res = {ccd_info}
		res.blkl = 512l
		res.linl = 80l
		res.pixsiz = 0.091
		rhead = res.head
		tags = tag_names(rhead)
		tlen = strlen(tags)
		openr, icun, wfnam, /get_lun
		repeat begin
			hlen = res.blkl*blkn/res.linl
			head = strarr(hlen)
			rform = strcompress('('+string(hlen)+'a'+string(res.linl)+')',/rem)
			readf, icun, head, form = rform
			loc = Strmatch_mm(tags[0],head,tlen[0])
			if loc ge 0 then begin
				dum = Strparse_mm(head[loc],' :	',lis)
				tem = fix(lis[dum])
			endif else begin
				free_lun, icun
				message, 'File corrupted, exiting!'
			endelse
			if tem gt blkn then begin
				blkn = tem
				hdone = 0
				point_lun, icun, 0
			endif else hdone = 1
		endrep until hdone
		free_lun, icun
		rhead.(0) = blkn

		nam = strtrim(strmid(head,0,7),2)
		val = strtrim(strmid(head,8),2)
		for i = 1l, n_tags(rhead)-1 do begin
			j = Strmatch_mm(tags[i],nam,tlen[i])
			if dum ge 0 then rhead.(i) = val[j]
		endfor
	endif else message, 'Bad or missing file!'

	res.head = rhead
	return, res
end