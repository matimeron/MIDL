Function Wedge_scan, img, imgstruct= istr, center= cnt, angle= ang, range= ran,$
	focal = foc, degrees = deg, step = stp, nsteps = nst, show_progress = sho, $
	normalize = nrm, strip = str, scan_steps = nval, _extra = _e

	on_error, 1

	shofl = keyword_set(sho)
	nstdef = 20
	if keyword_set(deg) then amul = !dtor else amul = 1.

	if (Wherinstruct('rad',_e))[0] ge 0 then message,'"keyword RAD not allowed!'

	if n_elements(ran) eq 0 then message, 'Radial range is needed!' $
	else if n_elements(ran) eq 1 then wran = [0.,ran] else wran = 1.*ran

	if Type(istr) eq 8 then begin
		cnt = Default(cnt,istr.cent)
		foc = Default(foc,istr.foc)
		if n_elements(ang) ne 0 then wang = ang*amul else wang = [0,2*!pi]
		strufl = 1
	endif else strufl = 0

	if One_of(stp,nst) le 0 then begin
		if Isnum(stp) then begin
			dwran = max(wran,min=min) - min
			nval = ceil(dwran/stp) > 1
			wran[1] = wran[0] + nval*stp
		endif else nval = nstdef
	endif else nval = nst > 1

	rads = Make_grid(wran,nval+1)
	res = Make_array(2,nval,typ=Calctype(img,1.))
	res[0,*] = 0.5*(rads[0:nval-1] + rads[1:nval])

	siz = size(img)
	if shofl then begin
		wimg = Byte_img(img)
		if !d.window eq (-1) then wset
		wshow
	endif

	if strufl then dum = Consec(size=siz,foc=foc,cent=cnt,ang=wang,$
			excent=istr.excent,rot=istr.rot,/new,/keep,_extra=_e) else $
	dum = Consec(size=siz,foc=foc,cent=cnt,ang=ang,deg=deg,/new,/keep,_extra=_e)
	for i = 0, nval-1 do begin
		dum = Conrep(rad=rads[i:i+1],foc=foc,count=rcon)
		if rcon gt 0 then res[1,i] = total(img[dum])/rcon
		if shofl and rcon gt 0 then begin
			wimg[dum] = wimg[dum] + 128b
			tvscl, wimg
			wimg[dum] = wimg[dum] + 128b
		endif
	endfor
	if shofl then tvscl, wimg

	if keyword_set(nrm) then begin
		tot = total(res[1,*])
		if tot ne 0 then res[1,*] = res[1,*]/tot
	endif
	If Isnum(str) then res[1,*] = Poly_filter(res,ord=str)

	return, res
end
