Pro UN_edit, name = nam, nperiods = nper, rad_sigma = rsig, ang_sigma = asig, $
	gscale = gsca, file = fname, outfile = ofname

	on_error, 1
	u = UN_struct()

	fname=file_get(fname,filt='dat',path=getenv('sruff_data'),/pick,stat=stat)
	if stat then begin
		ds = sdep(/ds)
		if n_elements(ofname) eq 0 then begin
			if Streq(strmid(fname,0,1),ds) then pds = ds else pds = ''
			nl = Strparse_mm(fname,ds,lis)
			lis[nl] = 'e_' + lis[nl]
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
	u = Unpri_sto(u,u.k,u.lamb,u.gamm,nper,name=nam)
	u = Unsec_sto(u,rsig,asig,gscale=gsca)
	writeu, outun, u

	glob = fltarr(2*u.nxy(0)+1,2*u.nxy(1)+1)
	point_lun, -datun, off

	idat = assoc(datun,glob,off)
	odat = assoc(outun,glob,off)
	for i = 0, u.nh do odat(i) = idat(i)

	free_lun, datun
	free_lun, outun
	return
end