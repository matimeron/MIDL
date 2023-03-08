Pro PD_gixos_time, snum, hroi= hroi, vroi= vroi, raw=raw, angle=ang, time=tim,$
	drop = drp, fnorm = fnr, fneps = eps, last = lst, wnew = wnw, result = res,$
	_extra = _e

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common pd_gixos_time_keep, gexs, typ, cres, tit, utit, wnum
	on_error, 1

	bwnum = 16l
	bsiz = 32l
	xsi = 22*bsiz
	ysi = 30*bsiz
	poff = [0, 11.5]*bsiz

	if not keyword_set(lst) then begin
		typ = 1 - One_of(ang,raw)
		dat = Scan_PD_comb(snum,hroi=hroi,vroi=vroi,/norm,/other,_extra=_e)
		dsiz = size(dat)
		cres = fltarr(4,dsiz[1],dsiz[2])
		cres[2,*,*] = dat[*,*,0]
		cres[3,*,*] = dat[*,*,1]
		tit = fildat.name + ' scan #' + string(snum,form='(i0)')
		utit = ['frame #','pix.']

		xy = lindgen(dsiz[1])
		if not keyword_set(raw) then begin
			if keyword_set(tim) then begin
				xy = (xy+1)*Scan_column(snum,'seconds',/const)
				utit[0] = 'time (seconds)'
			endif
			zz = Scan_PD_lcoo(snum,dir='ver',ang=ang,_extra=_e)
			if keyword_set(ang) then utit[1] = 'Beta' else utit[1] = 'Q!dz!n'
		endif else zz = lindgen(dsiz[2])
		if n_elements(vroi) eq 2 then zz = zz[vroi[0]:vroi[1]]
		cres[0,*,*] = xy#replicate(1.,dsiz[2])
		cres[1,*,*] = replicate(1.,dsiz[1])#zz
		gexs = 1
	endif else if not Isnum(gexs) then message, 'There is no previous data!'

	res = cres
	if keyword_set(fnr) then begin
		deps = min((res[2,*,*])[where(res[2,*,*] gt 0)])
		weps = Default(eps,deps,/dtyp)
		wfnr = (fnr - 1) > 0
		tem = replicate(1.,dsiz[1])#(reform(res[2,wfnr,*]) > weps)
		res[2,*,*] = res[2,*,*]/tem
		res[3,*,*] = res[3,*,*]/tem 		
		norow = total(reform(res[2,*,*]),2)/dsiz[2]
		tem = (norow > min(norow[where(norow gt 0)]))#replicate(1.,dsiz[2])
		res[2,*,*] = res[2,*,*]/tem
		res[3,*,*] = res[3,*,*]/tem 
	endif

	if keyword_set(wnw) $
	then wnum = bwnum + ((Default(wnum,bwnum-1) - bwnum + 1) mod 4) $
	else wnum = Default(wnum,bwnum)
	window, wnum, xsiz = xsi, ysiz = ysi, $
	tit = strcompress('IDL ' + string(wnum) + ' : PD GIXOS TIME Results')
	Display_mm, res, /auz, poff= poff, tit= tit, xtit= utit[0], ytit= utit[1], $
	_extra = _e

	drp = Default(drp,[0,0])
	res = Img_int(res, /z_int, _extra = _e)
	len = (size(res))[2]
	res = res[*,drp[0]:len-1-drp[1]]
	rmin = min(res[1,*])
	ares = res
	ares[1,*] = 1 - rmin/res[1,*]
	ares[2,*] = ares[1,*]*sqrt((res[2,*]/rmin)^2 + (res[2,*]/res[1,*])^2)

	pos = [2*bsiz,2*bsiz,10*bsiz,11*bsiz]
	Scan_show, res, pos=pos, /dev, /noerase, _extra = _e
	pos = [13*bsiz,2*bsiz,21*bsiz,11*bsiz]
	Scan_show, ares, pos=pos, /dev, /noerase, _extra = _e

	Wimg_mm, Clean_name(tit), call = 2, /nodef, /verb, _extra = _e

	return
end