Pro PD_gixos_old, snum, noback = nbc, slit = sli, ssize = ssz, $
	angles = ang, shift = shf, scale = scl, tiny = tin, wnew = wnw, $
	show_factors = sho, result = res, sfactor = sfc, bfactor = bfc, _extra = _e

;+
; NAME:
;		PD_GIXOS
; VERSION:
;		8.01
; PURPOSE:
;		Patches together PD GIXOS scans and subtracts background.
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		PD_GIXOS, SNUM [, keywords])
; INPUTS:
;	SNUM
;		Scan number or a list of scan numbers, in any form acceptable by
;		RANGE_PROC.  May include both signal and background scans.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/NOBACK
;		Switch.  If set, it is assumed that only signal is present, no
;		background.
;	SLIT
;		Numeric scalar, an optional electronic slit size, in mm.  The size will
;		be modified internally to correspond to an odd number of pixels.
;	/SSIZE
;		Switch.  If set, the final working slit size is displayed.  If SLIT is
;		not given, /SSIZE has no effect.
;	/ANGLES
;		Switch.  If set the result is given as function of the vertical angle
;		BETA instead of Q_z.
;	SHIFT
;		Scalar value, the shift in Q_xy (or BETA, if /ANGLES is set) to be
;		applied to the background.  Default value is 0.
;	SCALE
;		Scaling factor to be applied to the background.  Default value is 1.
;	/TINY
;		Switch.  If set, the display window is reduced in size, making it
;		appropriate for small laptops.
;	/WNEW
;		Switch.  If set, a new graphics window is created instead of the old
;		one being overwritten.  The windows at the disposal of PD_GIXOS are 
;		20-23, when the last one is reached the sequence reverts to the origin.
;	/SHOW_FACTORS
;		Switch.  If set, the patching factors are printed to the screen.
;	RESULT
;		Optional output, see below.
;	SFACTOR
;		Optional output, see below.
;	BFACTOR
;		Optional output, see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to embedded
;		routines.  This includes (among others) the keywords LO_CUT, HI_CUT and
;		FORCE, which modify the patching.  See PD_PATCH_GEN for details.
;
; OUTPUTS:
;		Standard output is plot only.  Optional output through keywords.
;
;		Note:	Graphics output is possible by using one of the keywords: /JPEG,
;		/PNG or /BMP.
; OPTIONAL OUTPUT PARAMETERS:
;	RESULT
;		Returns the patched and background subtracted data in the standard [3,*]
;		form:
;
;		Column 0	:	Q_z (or Beta if /ANGLES is set)
;		Column 1	:	intensity.
;		Column 2	:	errors.
;	SFACTOR
;		Returns the patching factors for the signal scans.  See FACTOR in
;		SCAN_JOIN fro details.
;	BFACTOR
;		Same as SFACTOR, for background scans.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
;		PD_GIXOS_KEEP.  Contains the index of the last graphics window used.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other that the data files must exist.
; PROCEDURE:
;		Standard patching using PD_PATCH_GEN.  Calls IMG_INT, SCAN_CLEAN, 
;		SCAN_COLUMN, SCAN_FIELD_READ, SCAN_PD_CENTER, SCAN_PD_READ, SCAN_SCALE,
;		SCAN_SHOW and SPEC_FILE_CHECK.  Calls CLEAN_NAME, DEFAULT, FLTROUND, 
;		ISNUM, LEGEND_MM, PLVAR_KEEP, RANGE_COMP, SORPURGE, TABULATE and 
;		WIMG_MM, from MIDL.
; MODIFICATION HISTORY:
;		Created once upon the time by Binhua Lin.
;		Rewritten 20-APR-2009 by Mati Meron.
;		Modified 20-JUN-2009 by Mati Meron.  Internal changes. Added keywords
;		/NOBACK, /WNEW, SFACTOR and BFACTOR.
;		Modified 25-JUN-2009 by Mati Meron.  More internal changes.
;		Modified 10-AUG-2009 by Mati Meron.   Added electronic slit capability,
;		using keywords SLIT and  SSIZE.
;		Modified5-NOV-2009 by Mati Meron.  Internal changes.
;		Modified 20-MAR-2010 by Mati Meron.  Internal changes.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;		Modified 25-AUG-2010 by Mati Meron.  Replaced LEGEND with LEGEND_MM.
;		Modified 20-FEB-2011 by Mati Meron.  Internal changes.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	common pd_gixos_keep, lwin
	on_error, 1

	nmx = 4
	bsiz = 32l
	xsi = [16,16,16]*bsiz
	ysi = [24,20,13]*bsiz
	if keyword_set(ang) then xtit = 'Beta' else xtit = 'Q!dz!n'
	if keyword_set(nbc) then begin
		xsi = xsi[2]
		ysi = ysi[2]
		bfl = 0
	endif else begin
		tifl = keyword_set(tin)
		xsi = xsi[tifl]
		ysi = ysi[tifl]
		bfl = 1
	endelse
	owin = !d.window
	bwnum = 20l
	if keyword_set(wnw) then begin
		lwin = Default(lwin,bwnum-1)
		wnum = bwnum + ((lwin - bwnum + 1) mod 4)
	endif else wnum = Default(lwin,bwnum)
	lwin = wnum

	Spec_file_check, snum, par= 'Det_th', /pil, /close, lis= wsnum
	bet = reform((Scan_field_read(wsnum,'angs'))[*,1])
	rbet = Fltround(bet,dig=3)
	sor = Sorpurge(rbet,net=nn)
	sbet = rbet[sor]
	if bfl then begin
		if nn mod 2 eq 0 then begin
			ns = nn/2
			for i = 0, ns - 1 do begin
				bind = where(rbet eq sbet[2*i],bhm)
				sind = where(rbet eq sbet[2*i+1],shm)
				if i eq 0 then begin
					blis = wsnum[bind]
					bnum = bhm
					slis = wsnum[sind]
					snum = shm
				endif else begin
					blis = [blis,wsnum[bind]]
					bnum = [bnum,bhm]
					slis = [slis,wsnum[sind]]
					snum = [snum,shm]
				endelse
			endfor
			bnum = [0l,total(bnum,/cum,/pres)]
			snum = [0l,total(snum,/cum,/pres)]
		endif else message, "Numbers of signals and backgrounds don't agree!"
	endif else begin
		ns = nn
		for i = 0, ns -1 do begin
			sind = where(rbet eq sbet[i],shm)
			if i eq 0 then begin
				slis = wsnum[sind]
				snum = shm
			endif else begin
				slis = [slis,wsnum[sind]]
				snum = [snum,shm]
			endelse
		endfor
		snum = [0l,total(snum,/cum,/pres)]
	endelse
	cnorm = Scan_column(slis[0],'monc',/ave)

	window, wnum, xsize = xsi, ysize = ysi, $
	tit = strcompress('IDL ' + string(wnum) + ' : PD GIXOS Results')

	if Isnum(sli) then begin
		pix = Scan_field_read(wsnum,'pdpix',/const,cfl=cfl)
		if not cfl then message, 'Non constant pixel size!'	
		ct = (Scan_PD_center(slis[0]))[0]
		hs = round((sli/pix - 1)/2)
		hroi = ct + [-hs,hs]
		if keyword_set(ssz) then begin
			print
			print, '	Slit size is ' + $
			string((1+2*hs)*pix,form = '(f6.3," mm")')
			print
		endif
	endif else hs = -1

	sgnam =  strcompress('sg_' + sindgen(nmx),/remove)
	for i = 0, ns - 1 do begin
		for j = snum[i], snum[i+1]-1 do begin
			tem = Scan_PD_read(slis[j],ori='ver',/norm,/glob,/per,/bad,$
			hroi=hroi,/noint,angles= ang,_extra = _e)
			if j gt snum[i] then begin
				dat[2,*,*] = dat[2,*,*] + tem[2,*,*]
				dat[3,*,*] = sqrt(dat[3,*,*]^2 + tem[3,*,*]^2)
			endif else dat = tem
		endfor
		if hs eq 0 then dat = reform(dat[1:3,0,*]) $
		else dat = Img_int(dat,/xy_int,_extra=_e)
		dat = Scan_scale(dat,cnorm/(snum[i+1] - snum[i]))
		dum = execute(sgnam[i] + ' = dat')
	endfor
	sig = PD_patch_gen(sg_0,sg_1,sg_2,sg_3,fac=sfc,_extra=_e)
	lohi = alog10([min(sig[1,*],max=max)>1,max])
	yran = 10.^[floor(lohi[0]),ceil(lohi[1])]

	if bfl then begin
		bgnam =  strcompress('bg_' + sindgen(nmx),/remove)
		for i = 0, ns - 1 do begin
			for j = bnum[i], bnum[i+1]-1 do begin
				tem = Scan_PD_read(blis[j],ori='ver',/norm,/glob,/per,/bad,$
				/noint,angles= ang,_extra = _e)
				if j gt bnum[i] then begin
					dat[2,*,*] = dat[2,*,*] + tem[2,*,*]
					dat[3,*,*] = sqrt(dat[3,*,*]^2 + tem[3,*,*]^2)
				endif else dat = tem
			endfor
			dat = Scan_scale(Img_int(dat,/xy_int,_extra=_e),$
			cnorm/(bnum[i+1] - bnum[i]))
			dum = execute(bgnam[i] + ' = dat')
		endfor
		bac = PD_patch_gen(bg_0,bg_1,bg_2,bg_3,fac=bfc,_extra=_e)
		if Isnum(shf) then bac[0,*] = bac[0,*] + shf
		if Isnum(scl) then bac = Scan_scale(bac,scl)
		res = Scan_clean(sig, bac, /inter)

		zsig = [min(sig[0,*],max=max),max]
		zbac = [min(bac[0,*],max=max),max]
		zran = [zsig[0] > zbac[0], zsig[1] < zbac[1]]
		dum = where($
		(res[0,*] ge zran[0]) and (res[0,*] le zran[1]) and (res[1,*] gt 0))
		res = res[*,dum]

		Plvar_keep, act = 'sav'

		tit = fildat.name + '!c' + 'Signal: S# ' + Range_comp(slis,/space) + $
		'!c' + 'Background: S# ' + Range_comp(blis,/space)
		pos = [2*bsiz,ysi/2+bsiz,xsi-bsiz,ysi-2*bsiz]
		Scan_show, sig, bac, lcol = [!pcol.red,!pcol.blue], /ylog, yran = yran,$
		tit = tit, xtit = xtit, pos = pos, /dev, /nofile, _extra = _e
		Legend_mm, text = ['Signal', 'Background'], line = [0,0], $
		color = [!pcol.red,!pcol.blue]

		pos = [2*bsiz,2*bsiz,xsi-bsiz,ysi/2 - bsiz]
		Scan_show, sig, res, lcol = [!pcol.red,!pcol.cyan], /ylog, yran = yran,$
		xtit= xtit, pos = pos, /dev, /noerase, /nofile, _extra=_e
		Legend_mm, text = ['Raw Signal', 'Net Signal'], line = [0,0], $
		color = [!pcol.red,!pcol.cyan]

		Plvar_keep, act = 'res'
	endif else begin
		res = sig
		tit = fildat.name + '!c' + 'Signal: S# ' + Range_comp(slis,/space)
		pos = [2*bsiz,2*bsiz,xsi-bsiz,ysi-2*bsiz]
		Scan_show, sig, lcol = !pcol.red, /ylog, yran = yran,$
		tit = tit, xtit = xtit, pos = pos, /dev, /nofile, _extra = _e
	endelse

	if keyword_set(sho) then begin
		print
		Tabulate, /ind, sfc, bfc, tit = 'Patching Factors', $
		head = ['Signal','Background']
		print
	endif

	wshow, wnum
	Wimg_mm, Clean_name(tit), call = 2, /nodef, /verb, _extra = _e
	if owin ge 0 then wset, owin

	return
end