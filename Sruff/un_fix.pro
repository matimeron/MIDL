Pro UN_fix, thresh, up = up, file = fname, outfile = ofname, show = sho

	on_error, 1
	u = UN_struct()

	thresh = Default(thresh,2.,/dtype)
	fname=file_get(fname,filt='dat',path=getenv('sruff_data'),/pick,stat=stat)
	if stat then begin
		ds = sdep(/ds)
		if n_elements(ofname) eq 0 then begin
			if Streq(strmid(fname,0,1),ds) then pds = ds else pds = ''
			nl = Strparse_mm(fname,ds,lis)
			lis[nl] = 'f_' + lis[nl]
			ofname = pds + strjoin(lis,ds)
		endif else begin
			if strpos(ofname,'.') eq (-1) then ofname = ofname + '.dat'
			if strpos(ofname,ds) eq (-1) then $
			ofname = getenv('sruff_data') + ofname
		endelse
		openr, datun, fname, /get_lun, /block
		openw, outun, ofname, /get_lun, /block
	endif else message, 'Cannot find file!'
	readu, datun, u
	writeu, outun, u
	glob = fltarr(2*u.nxy(0)+1,2*u.nxy(1)+1)
	point_lun, -datun, off

	idat = assoc(datun,glob,off)
	odat = assoc(outun,glob,off)
	for i = 0, u.nh do begin
		tarr = idat(i)
		if i gt 0 then begin
			if keyword_set(up) then begin
				tarr = Deglitch(tarr,thresh,/up, count = con)
				if keyword_set(sho) then print, i, con, '   up'
			endif else begin
				tarr = Deglitch(tarr,thresh,count = con)
				if keyword_set(sho) then print, i, con, '   down'
			endelse
		endif
		odat(i) = tarr
	endfor

	free_lun, datun
	free_lun, outun
	return
end