Function Ring_scan, img, imgstruct= istr, center= cnt, radius= rad, range=ran,$
	focal = foc, degrees = deg, step = stp, nsteps = nst, show_progress = sho,$
	normalize = nrm, strip = str, scan_steps = nval, _extra = _e

	on_error, 1

	shofl = keyword_set(sho)
	nstdef = 20
	if keyword_set(deg) then amul = !dtor else amul = 1.

	if (Wherinstruct('ang',_e))[0] ge 0 then message,'"keyword ANG not allowed!'

	if n_elements(ran) eq 0 then wran =  [0, 2*!pi] $
	else if n_elements(ran) eq 1 then wran = amul*[0.,ran] else wran = amul*ran
	dwran = max(wran,min=min) - min
	if abs(dwran) ge 2*!pi - Toler() then begin
		dwran = 2*!pi
		wran = [0, dwran]
		cfl = 1
	endif else cfl = 0

	if Type(istr) eq 8 then begin
		cnt = Default(cnt,istr.cent)
		foc = Default(foc,istr.foc)
		strufl = 1
	endif else strufl = 0

	if One_of(stp,nst) le 0 then begin
		if Isnum(stp) then nval = round(dwran/(amul*stp))> 1 else nval = nstdef
	endif else nval = nst > 1

	angs = Make_grid(wran,nval+1)
	res = Make_array(2,nval,typ=Calctype(img,1.))
	res[0,*] = 0.5*(angs[0:nval-1] + angs[1:nval])

	siz = size(img)
	if shofl then begin
		wimg = Byte_img(img)
		if !d.window eq (-1) then wset
		wshow
	endif

	if strufl then dum = Consec(size=siz,foc=foc,cent=cnt,radius=rad,$
			excent=istr.excent,rot=istr.rot,/new,/keep,_extra=_e) else $
	dum = Consec(size=siz,foc=foc,cent=cnt,radius=rad,/new,/keep,_extra=_e)
	for i = 0, nval-1 do begin
		dum = Conrep(ang=angs[i:i+1],foc=foc,count=rcon)
		if rcon gt 0 then res[1,i] = total(img[dum])/rcon
		if shofl and rcon gt 0 then begin
			wimg[dum] = wimg[dum] + 128b
			tvscl, wimg
			wimg[dum] = wimg[dum] + 128b
		endif
	endfor
	if shofl then tvscl, wimg

	res[0,*] = res[0,*]/amul
	if keyword_set(nrm) then begin
		tot = total(res[1,*])
		if tot ne 0 then res[1,*] = res[1,*]/tot
	endif
	if Isnum(str) then if cfl then res[1,*] = HP_filter(res,str) $
	else message, 'Not a complete circle, "strip" operation not allowed.', /con

	return, res
end

