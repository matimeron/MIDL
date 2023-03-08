Pro UN_slice, cur, dist, rsig, asig, file = fname, gscale = gsca, $
	slice = sle, bandwidth = ban, harm_units = haru, $
	smear = sme, window = win, tilt_ang = tang, xtilt = xti, degrees = deg, $
	filters = filts, filthicks = filths, mirrors = mirrs, mirangles = mirans, $
	extra_process = ext, params = parm,$
	levels = slev, dense = den, surface = surf, wait = wai, outflux = oflux

	common un_consts, hcove, alpha, oovee, ecf

	on_error, 1
	u = UN_struct()

	cur = Default(cur,1e-3,/dtype)
	dist = Default(dist,1.,/dtype)

	fname=file_get(fname,filt='dat',path=getenv('sruff_data'),/pick,stat=stat)
	if stat then begin
		openr, datun, fname, /get_lun, /block
		readu, datun, u
	endif else message, 'Cannot find file!'

	glob = fltarr(2*u.nxy(0)+1,2*u.nxy(1)+1)
	point_lun, -datun, off
	udat = assoc(datun,glob,off)
	uarr = udat(0)
	ban = Default(ban,1e-3,/dtype)
	fmult = 1e-6*alpha*oovee*cur*ban*(u.nper*u.gamm/dist)^2
	efac = 8*ecf*hcove/u.lamb*(u.gamm/u.k)^2*uarr
	ehar = efac(u.nxy(0),u.nxy(1))

	x = dist*u.ganx(0:2*u.nxy(0))
	y = dist*u.gany(0:2*u.nxy(1))
	arel = abs((x(1)-x(0))*(y(1)-y(0)))

	nsl = n_elements(sle)
	if nsl eq 0 then begin
		wsle = [1]
		nsl = 1l
		haru = 1
	endif

	if not keyword_set(haru) then begin
		wsle = Cast([sle],4)
		sle = 1./ehar*wsle
	endif else wsle = ehar*[sle]
	sli = fltarr(nsl,2*u.nxy(0)+1,2*u.nxy(1)+1)

	smefl = keyword_set(sme)

	frame = Winframe(x,y,window=win,xygrid=wtem,windex=qxy,fringe=fri)

	if n_elements(tang) ne 0 then sang = $
	Tilt(tang,deg=deg,xv=x,yv=y,ar=arel,xy=wtem,pow=fmult,xti=xti,til=tilst)

	profl = 0
	idum = How_many(firs=filts,seco=filths,thir=mirrs,four=mirans,which = whi)
	if (whi eq 3 or whi eq 12 or whi eq 15) then profl = 1 else $
	if whi ne 0 then message, 'Faulty processing information!', /continue

	if Type(ext) eq 7 then extfl = 1 else extfl = 0

	wai = Default(wai,0,/dtype)
	if Strmatch_mm(!d.name,['mac','regis','sun','tek','win','x']) eq -1 then $
	wai = 0

	cafe = 1./efac
	hoff = max(ehar*frame*cafe) > 1
	seps = 5e-6
	sumr = 1./(!pi*u.nper*sqrt(2*seps))
	for j = 0l, nsl - 1 do begin
		loi = floor(sle(j) - sumr) > 1l
		hii = ceil(sle(j)*hoff + sumr) < u.nh
		farr = glob
		earr = replicate(wsle(j),2*u.nxy(0)+1,2*u.nxy(1)+1)
		for i = loi, hii do farr = $
			farr + UH_band_ave(wsle(j)*cafe,i,u.nper,ban)*udat(i)
		farr = fmult*farr
		if profl then farr = $
			UN_process(whi,earr,farr,filts,filths,mirrs,mirans)
		if extfl then farr = call_function(ext,earr,farr,wtem,sang,para=parm)
		if smefl then farr = Smear(farr,u,dist,smearst=smest)
		farr = farr*frame
		sli(j,*,*) = farr
	endfor

	Plvar_keep, act = 'sav', /sho
	!y.margin = [6,4]

	rat = 10
	if keyword_set(den) then rat = sqrt(rat)
	for j = 0l, nsl - 1 do begin
		farr = reform(sli(j,*,*))

		Gentit, u, cur, dist, ban, quan = 'flux', $
			total = arel*Partot(farr,syme=qxy,symf=fri), peak = max(farr),$
			smear = smest, tilt = tilst, surface = surf, $
			titl = tit, subtitl = subtit, xtit = xtit, ytit = ytit, ztit = ztit
		tit(1) = tit(1) + $
			'E =' + strcompress(string(wsle(j),form="(g9.4,' keV')")) + $
			'  (E/E!D1!N ='+strcompress(string(wsle(j)/ehar,form='(f6.3)'))+')'
		subtit(1) = subtit(1) + '.'

		if keyword_set(surf) then begin
			surface, farr, x, y, xtit = xtit, ytit = ytit, ztit = ztit, $
			title = tit(0) + tit(1), subtitle = subtit(0) + subtit(1)
		endif else begin
			levs = Conlevs(farr,rat,/ignore,annot=annot,lines=lin,show=slev)
			contour, farr, x, y, $
			levels = levs, c_line = lin, c_annotation = annot, $
			xtit = xtit, ytit = ytit, $
			title = tit(0) + tit(1), subtitle = subtit(0) + subtit(1)
		endelse
		if j ne nsl - 1 then wait, wai
	endfor

	Plvar_keep, act = 'res'

	oflux = make_array(nsl + 2, 2*u.nxy(0) + 1, 2*u.nxy(1) + 1, /float)
	oflux(0:1,*,*) = wtem
	oflux(2:nsl+1,*,*) = sli

	free_lun, datun

	return
end