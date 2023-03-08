Pro Det_cal, range, inp_dir= idr, exten= ext, func= fun, show= sho, wait= wai, $
	res = res, _extra = _e

	on_error, 1

	dext = '.sfrm'
	ext = Default(ext,dext,/dtyp)
	idir = File_get(idr,/dir,title='Select input folder',_extra=_e)
	flist = file_search(idir+'*'+ext,count=nfil)
	if nfil eq 0 then begin
		message, 'No data files found!', /con
		return
	end

	res = {nfil: nfil, dat: replicate({peak_pars},nfil)}
	shofl = Default(sho,0,/dtyp)
	wai = Default(wai,1.)
	for i = 0, nfil-1 do begin
		res.dat[i]= $
		PD_peak_pars(range=range,file=flist[i],fun=fun,sho=shofl,/aut,_extra=_e)
		if shofl then wait, wai
	endfor

	return
end