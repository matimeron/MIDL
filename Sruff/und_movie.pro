Pro Und_movie, current= cur, distance= dst, ene_range= eran, step= stp, $
	ufile= ufil, slit= sli, bandwidth= ban, up= up, down= down, wait= wai, $
	save= sav, mfile= mfil, rate= rat, _extra= _e

	common un_consts, hcove, alpha, oovee, ecf
	common umov_file, lpath

	on_error, 1
	bsiz = 32
	brep = 30
	if Type(lpath) eq 0 then lpath = '' 
	dum = How_many(fir=ufil,sec=mfil,whi=whif)
	ufil=file_get(ufil,filt='dat',path=getenv('sruff_data'),/pick,stat=stat)
	if stat then begin
		u = UN_struct()
		openr, datun, ufil, /get_lun, /block
		readu, datun, u
		free_lun, datun
	endif else message, 'Cannot find file!'
	e1 = 2*ecf*hcove*u.gamm^2/u.lamb/(1 + u.k^2/2)
	BL_defaults, cur= cur, ape= ape, dist= dst

	if n_elements(eran) eq 2 then begin
		whi = One_of(down,up) > 0
		weran = [max(eran,min=min),min]
		if whi then weran = reverse(weran)
		ene = Make_grid(weran,stp,/step,dim=n)
	endif else message, 'Range needs 2 elements!'
	n = n[0]

	gape = dst/!blpar.dist*ape/2
	if keyword_set(sli) then begin
		if n_elements(sli) eq 2 then begin
			hape = sli/2. < gape
			rad = 0.
		endif else message, 'Slit needs two elements!'
	endif else begin
		hape = gape
		rad = min(hape)
	endelse
	mrk = [-hape,hape]

	tit = string(u.k,form='("K = ",f5.3)') + $
	string(round(1e3*cur),form='(" ; I = ",i0,"mA")') + $
	string(dst,form='(" ; dist = ",f6.3,"m")') + $
	string(e1,form='("!cE!d1!n = ",f6.3,"keV")')

	Und_slice, cur, dst, file= ufil, sli= ene, ban= ban, show= 0, $
	out= out, pflux= pflux, _extra= _e
	imax = max(pflux)
	dim = size(out,/dim)
	scal = 512/dim[1]
	x = reform(out[0,*,0])
	y = reform(out[1,0,*])
	arel = abs((x[1]-x[0])*(y[1]-y[0]))
	tflux = (uflux = pflux)
	dum = Winframe(x,y,window=gape,windex=gqxy,fringe=gfri)
	dum = Winframe(x,y,window=hape,windex=hqxy,fringe=hfri)
	for i = 0,n_elements(ene)-1 do begin
		tflux[i] = arel*Partot(reform(out[i+2,*,*]),syme=gqxy,symf=gfri)
		uflux[i] = arel*Partot(reform(out[i+2,*,*]),syme=hqxy,symf=hfri)
	endfor

	Display_mm, out, ind= 2, zoom= scal, wsi= wsi, isi= isi, /nodata
	pad = 16
	pysi = wsi[1]*3/4 < (1024 - wsi[1] - 2*pad)
	wsi = wsi + [2*pad,2*pad+pysi]
	wsi = (wsi+1)/2*2
	pos = [6*pad,3*pad,isi[0]+6*pad,pysi-1*pad]
	window, 24, xsize = wsi[0], ysize = wsi[1]
	wai = Default(wai,-1e-6,/dtyp)
	if wai gt 0 then print, string([13b,9b,9b]) + $
	'Hit "Q" to exit, "P" for previous, any other key to continue'

	if keyword_set(sav) and wai le 0 then begin
		mfil = File_get( $
			mfil,path=lpath,stat=stat,/write,/over,def='mp4',/auto,_extra=_e)
		dum = Fnamparse(mfil,path=lpath)
		vob = IDLffVideoWrite(mfil)
		vids = vob.AddVideoStream(wsi[0],wsi[1],30)
		wrep = round(brep/Default(rat,1)) > 1
		sfl = 1
	endif else sfl = 0

	_ee = _e
	xcheck = (Wherinstruct('xran',_e))[0]
	if xcheck ge 0 then _ee.(xcheck) = 0
	ycheck = (Wherinstruct('yran',_e))[0]
	if ycheck ge 0 then _ee.(ycheck) = 0
	for i = 0l, n-1 do begin
		Display_mm, out, ind= i+2, /auz, poff=[2*pad,pysi], mark=mrk, rad=rad,$
		zoom= scal, amax= imax, xtit = 'X (mm)', ytit = 'Y (mm)', $
		tit = tit + string(ene[i],form = '(" ; E = ",f6.3,"keV")'), $
		charsize= 1.4, charthi= 1.5, _extra = _ee
		plot, ene, tflux, position=pos, /noerase, /nodata,/dev, xtit='E (keV)',$
		ytit = string(ban,form='("Photon flux (ph/s/mm!e2!n/",f7.5,"bw")'), $
		thi=2, charsize=1.4, charthi=1.5, _extra = _e
		oplot, ene, tflux, thi = 2, col = !pcol.blue
		oplot, ene, uflux, thi = 2, col = !pcol.green
		plots, ene[i],tflux[i],psym=-8,col=!pcol.red,thi=2,symsize=1.5
		plots, ene[i],uflux[i],psym=-8,col=!pcol.red,thi=2,symsize=1.5,noclip=0
		
		if sfl then for j = 0, wrep-1 do dum = vob.Put(vids,TVRD(true=1))
		if wai gt 0 then begin
			dum = (dum = get_kbrd())
			if Streq(dum,'p',1) then i = (i-2) > (-1) $
			else if Streq(dum,'q',1) then break
		endif else if wai lt 0 then wait, abs(wai)
	endfor
	if sfl then vob.Cleanup

	Nullify, whif, fir= ufil, sec= mfil
	return
end