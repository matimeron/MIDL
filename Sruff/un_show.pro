Pro UN_show, cur, dist, rsig, asig, file = fname, gscale = gsca, $
	smear = sme, window = win, tilt_ang = tang, xtilt = xti, degrees = deg, $
	filters = filts, filthicks = filths, mirrors = mirrs, mirangles = mirans, $
	erange = eran, extra_process = ext, params = parm,$
	showset = shoset, pow_perh = poh, levels = slev, surface = surf, $
	wait = wai, no_show = nosh, outpower = opow, enpow = epo

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
	gmult = 8e-6*alpha*hcove*cur*u.nper*u.gamm^4/(u.lamb*u.k^2*dist^2)*uarr
	efac = 8*ecf*hcove/u.lamb*(u.gamm/u.k)^2*uarr
	ehar = efac(u.nxy(0),u.nxy(1))

	x = dist*u.ganx(0:2*u.nxy(0))
	y = dist*u.gany(0:2*u.nxy(1))
	arel = abs((x(1)-x(0))*(y(1)-y(0)))

	smefl = keyword_set(sme)

	frame = Winframe(x,y,window=win,xygrid=wtem,windex=qxy,fringe=fri)

	sang = $
	Tilt(tang,deg=deg,xv=x,yv=y,ar=arel,xy=wtem,pow=gmult,xti=xti,til=tilst)

	if n_elements(eran) eq 2 then begin
		eran = Cast(eran,4)
		erafl = 1
	endif else erafl = 0

	profl = 0
	idum = How_many(firs=filts,seco=filths,thir=mirrs,four=mirans,which = whi)
	if (whi eq 3 or whi eq 12 or whi eq 15) then profl = 1 else $
	if whi ne 0 then message, 'Faulty processing information!', /continue

	if Type(ext) eq 7 then extfl = 1 else extfl = 0

	wai = Default(wai,0,/dtype)
	if Strmatch_mm(!d.name,['mac','regis','sun','tek','win','x']) eq -1 then $
	wai = 0

	shofl = not keyword_set(nosh)
	if shofl then shoset = Default(shoset,0l,/dtype) else shoset = 0l
	if shoset(0) lt 0 then shoset = u.nh
	if (size(shoset))(0) eq 0 then $
	if shoset ne 0 then shoset = 1 + lindgen(shoset)
	powarr = fltarr(u.nh + 1)
	epo = fltarr(2,u.nh + 1)

	Plvar_keep, act = 'sav', /sho
	!y.margin = [6,4]

	for i = 1l, u.nh do begin
		parr = gmult*udat(i)
		if profl then parr = $
		UN_process(whi,i*efac,parr,filts,filths,mirrs,mirans)
		if extfl then parr = call_function(ext,i*efac,parr,wtem,sang,param=parm)
		if erafl then begin
			edum = where(efac lt eran(0)/i or efac gt eran(1)/i, nedum)
			if nedum ne 0 then parr(edum) = 0
		endif

		glob = glob + parr
		powarr(i) = arel*Partot(parr,syme=qxy,symf=fri)

		if (where(shoset eq u.harms(i)) ge 0)(0) then begin
			if smefl then parr = Smear(parr,u,dist,smearst=smest)
			parr = parr*frame
			totp = arel*Partot(parr,syme=qxy,symf=fri)

			Gentit, u, cur, dist, quan = 'area', total = totp, $
			peak = max(parr), smear = smest, tilt = tilst, surface = surf,$
			titl = tit, subtitl = subtit, xtit= xtit, ytit= ytit, ztit= ztit
			tit(1) = tit(1) + 'Harmonic #' + $
			strcompress(string(i,form='(i3)')) + ';  On_axis energy =' + $
			strcompress(string(i*ehar,form="(g9.4,' keV')")) + '.'
			subtit(1) = subtit(1) + '.'

			if keyword_set(surf) then begin
				surface, parr, x, y, xtit= xtit, ytit= ytit, ztit= ztit, $
				title = tit(0) + tit(1), subtitle = subtit(0) + subtit(1)
			endif else begin
				levs = Conlevs(parr,/ignore,annot=annot,lines=lin,show=slev)
				contour, parr, x, y, $
				levels = levs, c_line = lin, c_annotation = annot, $
				xtit= xtit, ytit= ytit, $
				title = tit(0) + tit(1), subtitle = subtit(0) + subtit(1)
			endelse
			wait, wai
		endif
	endfor

	rtotp = arel*Partot(glob*frame,syme=qxy,symf=fri)
	if smefl then glob = Smear(glob,u,dist,smearst=smest)
	glob = glob*frame
	totp = arel*Partot(glob,syme=qxy,symf=fri)

	if keyword_set(poh) and shofl then begin
		untem = byte(strcompress(u.name,/remove_all))
		if untem(0) ge 96 then untem(0) = untem(0) - 32
		tit = string(untem) + ' Harmonic power distribution at I =' + $
		strcompress(string(1000*cur, form="(i4,'mA')")) + $
		'.  K =' + strcompress(string(u.k, form = '(f7.3)')) +'.'
		subtit = 'Total power =' + $
		strcompress(string(totp, form = "(g9.4,' Watt.')"))

		plot, u.harms(0:u.nh), powarr, $
		xtit = 'Harmonic #', ytit = 'Power (Watt)', tit = tit, subtit = subtit
		wait, wai
	endif

	if shofl then begin
		Gentit, u, cur, dist, quan = 'area', total = totp, peak = max(glob),$
		smear = smest, tilt = tilst, surface = surf, $
		title = tit, subtitle = subtit, xtit= xtit, ytit= ytit, ztit= ztit
		tit(1) = tit(1) + 'All Harmonics'
		subtit(1) = subtit(1) + '.'

		if keyword_set(surf) then begin
			surface, glob, x, y, xtit= xtit, ytit= ytit, ztit= ztit, $
			title = tit(0) + tit(1), subtitle = subtit(0) + subtit(1)
		endif else begin
			levs = Conlevs(glob,/ignore,annot=annot,lines=lin,show=slev)
			contour, glob, x, y, $
			levels = levs, c_line = lin, c_annotation = annot, $
			xtit= xtit, ytit= ytit, ztit= ztit, $
			title = tit(0) + tit(1), subtitle = subtit(0) + subtit(1)
		endelse
	endif

	Plvar_keep, act = 'res'

	opow = make_array(3,2*u.nxy(0) + 1, 2*u.nxy(1) + 1, /float)
	opow[0:1,*,*] = wtem
	opow[2,*,*] = glob

	epo[0,*] = findgen(u.nh + 1)*ehar
	epo[1,*] = totp/rtotp*powarr

	free_lun, datun
	return
end