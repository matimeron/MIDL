Pro UN_flux, cur, dist, rsig, asig, file = fname, gscale = gsca, $
	erange = eran, bandwidth = ban, harm_units = haru, integ_prec = prec, $
	density = den, spread = spre, smear = sme, window = win, $
	filters = filts, filthicks = filths, mirrors = mirrs, mirangles = mirans, $
	extra_process = ext, params = parm, outflux = oflux

	common un_consts, hcove, alpha, oovee, ecf

	on_error, 1
	u = UN_struct()

	cur = Default(cur,1e-3,/dtype)
	dist = Default(dist,1.,/dtype)
	posib = ['LOW','MEDIUM','HIGH']
	siglev = [2.,3.,4.]
	seps = [1e-1,2e-3,4e-5]
	sumrs = [0.,1.,2]
	dens = [1.,2.,4.]
	spres = [4.,2.,1.]
	prl = Strmatch_mm(prec,posib,2) > 0
	dnl = Strmatch_mm(den,posib,2) > 0
	spl = Strmatch_mm(spre,posib,2) > 0

	fname=file_get(fname,filt='dat',path=getenv('sruff_data'),/pick,stat=stat)
	if stat then begin
		openr, datun, fname, /get_lun, /block
		readu, datun, u
	endif else message, 'Cannot find file!'

	glob = fltarr(2*u.nxy(0)+1,2*u.nxy(1)+1)
	point_lun, -datun, off
	udat = assoc(datun,glob,off)

	x = dist*u.ganx(0:2*u.nxy(0))
	y = dist*u.gany(0:2*u.nxy(1))
	steps = abs([x(1)-x(0),y(1)-y(0)])
	arel = steps(0)*steps(1)

	smefl = keyword_set(sme)
	frame = Winframe(x,y,window=win,xygrid=wtem,windex=qxy,fringe=fri)
	hoframe = frame

	if smefl then begin
		frame = Smear(frame,u,dist,smearst=smest)
		nxy = $
		ceil((siglev(prl)*sqrt(u.rsig^2+ (dist*u.asig)^2)+ win)/steps) < u.nxy
		qxy = nxy
		fri = 0*nxy
	endif else nxy = qxy

	lonxy = u.nxy - nxy
	hinxy = u.nxy + nxy
	frame = frame(lonxy(0):hinxy(0),lonxy(1):hinxy(1))
	hoframe = hoframe(lonxy(0):hinxy(0),lonxy(1):hinxy(1))
	wtem = wtem(*,lonxy(0):hinxy(0),lonxy(1):hinxy(1))

	stodat = fltarr(2*nxy(0)+1,2*nxy(1)+1,u.nh+1)
	for i = 0, u.nh do stodat(*,*,i) = $
	(udat(i))(lonxy(0):hinxy(0),lonxy(1):hinxy(1))

	ban = Default(ban,1e-3,/dtype)
	fmult = 1e-6*alpha*oovee*cur*ban*(u.nper*u.gamm/dist)^2
	efac = 8*ecf*hcove/u.lamb*(u.gamm/u.k)^2*reform(stodat(*,*,0))
	ehar = efac(nxy(0),nxy(1))

	neran = n_elements(eran)
	if keyword_set(haru) or neran eq 0 then harum = 1. else harum = 1./ehar
	if neran eq 0 then weran = u.harms([1,u.nh]) + 0.5*[-1,1] else $
	if neran eq 1 then weran = [harum*eran,u.harms(u.nh) + 0.5] else $
	weran = harum*eran

	ngr = u.nper*dens(dnl)
	kl = round(ngr/spres(spl)) > 3
	k = findgen(2*kl)
	frac = k/(2*kl)*(1 - (1 - 1.*kl/ngr)/((kl-1)*(2*kl-1))*((kl-k)*(2*kl-k)))
	frac = 1./(4*ngr)*round(4*ngr*frac)
	lfrac = 2*kl

	nsec = floor(weran(1)) - floor(weran(0)) + 1
	sle = fltarr(lfrac*nsec)
	for i = 0, nsec- 1 do sle(lfrac*i:lfrac*(i+1)-1) = i+ frac+ floor(weran(0))
	tem = where(sle ge weran(0) and sle le weran(1),nsl)
	sle = sle(tem)
	wsle = ehar*sle
	wban = sqrt(ban^2 + sin(!pi*sle)^2*((1./ngr^2 - ban^2) > 0))
	oflux = fltarr(nsl)

	profl = 0
	idum = How_many(firs=filts,seco=filths,thir=mirs,four=mirans,which = whi)
	if (whi eq 3 or whi eq 12 or whi eq 15) then profl = 1 else $
	if whi ne 0 then message, 'Faulty processing information!', /continue

	if Type(ext) eq 7 then extfl = 1 else extfl = 0

	cafe = 1./efac
	hoff = max(ehar*hoframe*cafe) > 1
	sumr = 1./((!pi*u.nper)^2*seps(prl)) + sumrs(prl)
	garr = fltarr(2*nxy(0)+1,2*nxy(1)+1)

	for j = 0l, nsl - 1 do begin

		loi = floor(sle(j) - sumr) > 1l
		hii = ceil(sle(j)*hoff + sumr) < u.nh

		if abs(sle(j) - round(sle(j))) lt Toler(1) then begin
			print, nsl-1, j, sle(j)
			print, loi, hii
			print
		endif

		farr = garr
		for i = loi, hii do farr = $
			farr + UH_band_ave(wsle(j)*cafe,i,u.nper,wban(j))*stodat(*,*,i)
		farr = fmult*reform(farr)
		if profl then farr = $
			UN_process(whi,wsle(j),farr,filts,filths,mirrs,mirans)
		if extfl then farr= call_function(ext,wsle(j),farr,wtem,sang,param=parm)
		oflux(j) = arel*Partot(farr*frame,syme=qxy,symf=fri)
	endfor

	Plvar_keep, act = 'sav', /sho
	!y.margin = [6,4]

	plot, wsle, oflux

	Plvar_keep, act = 'res'

	oflux = [[wsle],[oflux]]

	free_lun, datun

	return
end