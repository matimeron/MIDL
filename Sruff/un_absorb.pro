Pro UN_absorb, cur, dist, rsig, asig, file = fname, gscale = gsca, $
	smear = sme, window = win, tilt_ang = tang, xtilt = xti, degrees = deg, $
	filters = filts, filthicks = filths, mirrors = mirrs, mirangles = mirans, $
	extra_process= ext, params= parm, absorber= absor, dens= den, depths= dep, $
	levels = slev, surface = surf, show = sho, no_show = nosh, wait = wai, $
	save = sav, savfile = svname, outdata = odat

	common un_consts, hcove, alpha, oovee, ecf
	common sxr_stuff, sorlist, abctab, enun, denun, curbeam

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

	z = Default(dep,[0.],/dtype)
	if z(0) gt 0 then z = [0.,z]
	if n_elements(z) eq 1 then z = [z,1/Toler()]
	nz = n_elements(z)
	nexz = z[1:nz-1]
	prez = z[0:nz-2]
	thiz = nexz - prez
	pod = fltarr(nz-1,2*u.nxy(0)+1,2*u.nxy(1)+1)

	smefl = keyword_set(sme)

	frame = Winframe(x,y,window=win,xygrid=wtem,windex=qxy,fringe=fri)

	if n_elements(tang) eq 0 then sang = 1. else sang = $
	Tilt(tang,deg=deg,xv=x,yv=y,pow=gmult,ar=arel,xy=wtem,xti=xti,til=tilst)

	profl = 0
	idum = How_many(firs=filts,seco=filths,thir=mirrs,four=mirans,which = whi)
	if (whi eq 3 or whi eq 12 or whi eq 15) then profl = 1 else $
	if whi ne 0 then message, 'Faulty processing information!', /continue

	if Type(ext) eq 7 then extfl = 1 else extfl = 0

	wai = Default(wai,0,/dtype)
	if Strmatch_mm(!d.name,['mac','regis','sun','tek','win','x']) eq -1 then $
	wai = 0

	for i = 1l, u.nh do begin
		parr = gmult*udat(i)
		if profl then parr = $
	    	UN_process(whi,i*efac,parr,filts,filths,mirrs,mirans)
		if extfl then parr = call_function(ext,i*efac,parr,wtem,sang,param=parm)
		abco = 0.1*Abs_coeff(i*efac, elem = absor, dens = den)
		for j = 0l, nz - 2 do pod(j,*,*) = pod(j,*,*) + $
			parr*(exp(-prez(j)/sang*abco)- exp(-nexz(j)/sang*abco))/thiz(j)
	endfor

	shofl = not keyword_set(nosh)

	Plvar_keep, act = 'sav', /sho
	!y.margin = [6,4]

	for j = 0l, nz - 2 do begin
		parr = reform(pod(j,*,*))
		if smefl then parr = Smear(parr,u,dist,smearst=smest)
		parr = parr*frame
		pod(j,*,*) = parr
		iparr = thiz(j)*parr
		glob = glob + iparr

		if keyword_set(sho) and shofl then begin
			Gentit, u, cur, dist, quan = 'vol', $
			total = arel*Partot(iparr,syme=qxy,symf=fri), peak = max(parr),$
			smear = smest, tilt = tilst, surface = surf, $
			titl = tit, subtitl = subtit, xtit= xtit, ytit= ytit, ztit= ztit
			tit(1) = tit(1) + abctab(where(Streq(abctab.csym,absor,2))).name +$
			' absorber at depth of' + $
			strcompress(string(z(j),form="(g9.3,' mm')")) + $
			' (power absorbed within ' + $
			strcompress(string(prez(j),form="('[',g9.3,',')")) + $
			strcompress(string(nexz(j),form="(g9.3,']')")) + ' mm).'
			subtit(1) = subtit(1) + '.'

			if keyword_set(surf) then begin
				surface, parr, x, y, xtit = xtit, ytit = ytit, ztit = ztit, $
				title = tit(0) + tit(1), subtitle = subtit(0) + subtit(1)
			endif else begin
				levs = Conlevs(parr,/ignore,annot=annot,lines=lin,show=slev)
				contour, parr, x, y, $
				levels = levs, c_line = lin, c_annotation = annot, $
				xtit = xtit, ytit = ytit, $
				title = tit(0) + tit(1), subtitle = subtit(0) + subtit(1)
			endelse
			wait, wai
		endif
	endfor

	Gentit, u,cur,dist, quan= 'are', $
	total= arel*Partot(glob,syme=qxy,symf=fri), peak= max(glob),$
	smear = smest, tilt = tilst, surface = surf, $
	title = tit, subtitle = subtit, xtit= xtit, ytit= ytit, ztit= ztit
	tit(1) = tit(1) + abctab(where(Streq(abctab.csym,absor,2))).name + $
	' absorber.' + ' (power absorbed within ' + $
	strcompress(string(z(0),form="('[',g9.3,',')")) + $
	strcompress(string(z(nz-1),form="(g9.3,']')")) + ' mm).'
	subtit(1) = subtit(1) + '.'

	if shofl then begin
		if keyword_set(surf) then begin
			surface, glob, x, y, xtit = xtit, ytit = ytit, ztit = ztit, $
				title = tit(0) + tit(1), subtitle = subtit(0) + subtit(1)
		endif else begin
			levs = Conlevs(glob,/ignore,annot=annot,lines=lin,show=slev)
			contour, glob, x, y, $
				levels = levs, c_line = lin, c_annotation = annot, $
				xtit = xtit, ytit = ytit, $
				title = tit(0) + tit(1), subtitle = subtit(0) + subtit(1)
		endelse
	endif

	Plvar_keep, act = 'res'

	if keyword_set(sav) or n_elements(svname) ne 0 then begin
		aprou = Gen_struct(fname,1)

		aprou.source = u

		aprou.proc.cur = cur
		aprou.proc.dist = dist
		aprou.proc.conv = smefl
		aprou.proc.win = win
		aprou.proc.ban = 0.
		aprou.proc.absor = string(absor,format='(a2)')
		aprou.proc.sin_ang = sang
		aprou.proc.xtilt = keyword_set(xti)
		ntem = n_elements(filts)
		if ntem gt 0 then begin
			aprou.proc.nfilts = ntem
			aprou.proc.filts(0:ntem-1) = string(filts,format='(a2)')
			aprou.proc.filths(0:ntem-1) = filths
		endif
		ntem = n_elements(mirrs)
		if ntem gt 0 then begin
			aprou.proc.nmirrs = ntem
			aprou.proc.mirrs(0:ntem-1) = string(mirrs,format='(a2)')
			aprou.proc.mirans(0:ntem-1) = mirans
		endif
		if extfl then begin
			aprou.proc.extra = string(ext,format='(a32)')
			ntem = n_elements(parm)
			if ntem gt 0 then begin
				aprou.proc.nparams = ntem
				aprou.proc.params(0:ntem-1) = parm
			endif
		endif
		aprou.dsets = nz-1
		aprou.dvals(0:nz-1) = z

		ds = sdep(/ds)
		if n_elements(svname) eq 0 then begin
			if Streq(strmid(fname,0,1),ds) then pds = ds else pds = ''
			nl = Strparse_mm(fname,ds,lis)
			lis[nl] = 'abs_' + lis[nl]
			svname = pds + strjoin(lis,ds)
		endif else begin
			if strpos(svname,'.') eq (-1) then svname = svname + '.dat'
			if strpos(svname,ds) eq (-1) then $
			svname = getenv('sruff_data') + svname
		endelse
		openw, outun, svname, /get_lun, /block
		writeu, outun, aprou
		point_lun, -outun, off
		odat = assoc(outun,uarr,off)
		for j = 0, nz - 2 do odat(j) = reform(pod(j,*,*))
		free_lun, outun
	endif

	odat = make_array(nz+1,2*u.nxy(0) + 1, 2*u.nxy(1) + 1, /float)
	odat[0:1,*,*] = wtem
	odat[2:nz,*,*] = pod
	for j = 0l, nz-2 do odat[j+2,*,*] = thiz[j]*odat[j+2,*,*]

	free_lun, datun
	return
end