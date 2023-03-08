Function LD_read, sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7, $
	norm = nrm, chan_range = chr, bin = bin, patch_range = ptr, angles = ang, $
	horizontal = hor, signed = sgn, relaxed = rel, verbose = vrb, title = tit, $
	_extra = _e

;+
; NAME:
;		LD_READ
; VERSION:
;		7.15
; PURPOSE:
;		Reads a set of LD spectra and combines them into an image
; CATEGORY:
;		Surface specific.
; CALLING SEQUENCE:
;		Result = LD_READ( SL_0, ..., [,keywords])
; INPUTS:
;	SL_0, SL_1, SL_2, SL_3, SL_4, SL_5, SL_6, SL_7
;		List of linear detector scans, provided in any form that is acceptable
;		by SCAN_LIST_VER.  If more than one input is provided, LD_READ will
;		attempt patching in the horizontal direction.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	NORM
;		Specifies data normalization.  If NORM is set (with nonzero numeric
;		value) the data is normalized to MONC.  This can be overriden by
;		giving NORM as character value in which case this value specifies
;		that column to be used for normalization.
;	CHAN_RANGE
;		The range of channels to use in the readout.  Default is full range
;		present in the data.
;	BIN
;		Specifies the bin size to be applied to each scan.  Default is 1, i.e.
;		no binning.  Any size provided is always rounded downward to the nearest
;		power of 2 (but the resulting binsize is never smaller than 1).
;		Note:	When both CHAN_RANGE and BIN are present, the CHAN_RANGE values
;				are taken as pre-binned values.
;	PATCH_RANGE
;		The range of channels to use in patching (only when more than one data
;		set is present).  Note that PATCH_RANGE values are taken as original
;		values, before CHAN_RANGE and BIN are applied.
;	/ANGLES
;		Switch.  If set the coordinates of the image are the angles DTHETA (for
;		horizontal) and BETA for vertical, instead of Q_xy and Q_z.
;	/HORIZONTAL
;		Switch.  If set, the LD is assumed to be positioned horizontally
;		(default is vertically).
;	/SIGNED
;		Switch.  If set, Q_xy is multiplied by the sign of the DTH angle.
;		/SIGNED has no effect when /ANGLES is set.
;	/RELAXED
;		Switch.  If set, the internal transformation from angles to Q-values is
;		unconstrained.  By default it is constrained to the internal data
;		region, to avoid edge inaccuracies.
;
;		Note:  In the previous version of LD, the default option was
;		unconstrained and the keyword /TIGHT had to be used to constrain it.
;		While /TIGHT is still recognized, it has no effect.
;	/VERBOSE
;		Switch.  If set and PATCH_RANGE is provided, the Q_z patching range is
;		printed to the screen (only when more than one group of scans is used).
;	TITLE
;		Optional output (and input), see below.
;	_EXTRA
;		A formal keyword, used to transfer additional keywords to
;		SCAN_LD_READ.  Not to be used directly.
; OUTPUTS:
;		Usually returns a 3D array of dimensions [4,NSCANS,NCHANNELS], such that
;			Result[0,*,*] = Q_xy coordinates of the image.
;			Result[1,*,*] = Q_z coordinates of the image.
;			Result[2,*,*] = The image itself.
;			Result[3,*,*] = The statistical errors of the image data.
;
;		In the special case when only a single scan is present (i.e. NSCANS = 1)
;		returns a 2D array of dimensions [3,NCHANNELS] such that
;			Result[0,*] = Q_z coordinates of the scan.
;			Result[1,*] = The scan data itself.
;			Result[2,*] = The statistical errors of the data.
;
;		However, see /ANGLES above for alternative coordinates.
; OPTIONAL OUTPUT PARAMETERS:
;	TITLE
;		If provided, returns the same, else returns a character scalar, made of
;		the file name and the scan numbers, to be used as title for plots.
; COMMON BLOCKS:
;		SPEC_FILE.  See the routine SPEC_FILE_INFO for description.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1)  The scan list provided must include only valid LD scans with same
;			number of channels.
;		2)	The value of LAMBDA must be constant for all the scans and equal
;			the calibration value (to within 1e-3 relative).
; PROCEDURE:
;		Reads the data, evaluates Q_z and Q_xy using calibration values, then
;		translates the Q values to rectalinear coordinates and interpolates
;		the data using triangulation.
;		Calls IMG_TRIG.  Calls SCAN_FIELD READ, SCAN_JOIN, SCAN_LD_READ,
;		SCAN_ORDER, SCAN_PAR_READ and SPEC_FILE_INFO from SPEC.  Calls IMG_TRIG.
;		Also calls ARREQ, DEFAULT, ISNUM, NPARDEF, SIGN and WHERINSTRUCT from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 25-NOV-2005 by Mati Meron.
;		Modified 5-DEC-2005 by Mati Meron.  Added keyword /TIGHT.
;		Modified 10-DEC-2005 by Mati Meron.  Internal changes.
;		Modified 15-DEC-2005 by Mati Meron.  Added keyword /ANGLES.
;		Modified 20-DEC-2005 by Mati Meron.  Added keyword NORM.
;		Modified 15-FEB-2006 by Mati Meron.  Added special treatment for
;		single scans.
;		Modified 20-FEB-2006 by Mati Meron.  Added option of multiple inputs
;		and patching.
;		Modified 7-MAR-2006 by Stephen Danauskas.  Added user defined title.
;		Modified 5-APR-2006 by Mati Meron.  Internal changes.
;		Modified 20-JUN-2006 by Mati Meron.  Changed output format to include
;		statistical errors.  Replaced keyword /TIGHT with /RELAXED.
;		Modified 10-OCT-2006 by Mati Meron.  Added keywords /SIGNED and /VERBOSE
;		Modified 10-NOV-2006 by Mati Meron.  Internal changes.
;		Modified 5-MAR-2007 by Mati Meron.  Added /HORIZONTAL option.
;		Modified 25-MAR-2010 by Mati Meron.  Common block update.
;-

	common spec_file, exs, fildat, n_s, n_m, n_l, n_smax, itop
	on_error, 1

	warn = 'Missing or non-constant '
	cwnam = ['detector distance.','chan/mm.','specular channel.','lambda','G_L']
	pnam = ['In_Rot','Out_Rot','Det_Th']
	dnam = ['Ldist','Lmpc','Lns','Lambda','G_L']

	Spec_file_info, _extra = _e
	dum = (Wherinstruct('new',_e))[0]
	if dum ge 0 then _e.(dum) = 0

	n = Npardef(sl_0, sl_1, sl_2, sl_3, sl_4, sl_5, sl_6, sl_7,whi=j)
	sarr = strarr(n)
	if n gt 1 then begin
		slnam =  strcompress('sl_' + sindgen(8),/remove)
		ldnam =  strcompress('ld_' + sindgen(8),/remove)
		linam =  strcompress('li_' + sindgen(8),/remove)
		zinam =  strcompress('zi_' + sindgen(8),/remove)
		oslnam =  strcompress('osl_' + sindgen(8),/remove)
		oldnam =  strcompress('old_' + sindgen(8),/remove)
		olinam =  strcompress('oli_' + sindgen(8),/remove)
		ozinam =  strcompress('ozi_' + sindgen(8),/remove)

		nxs = lonarr(n)
		si = sindgen(n)
		acom = '= Scan_ld_read('
		bcom = ',norm=nrm,chan=chr,bin=bin,list=lis,_extra=_e)'
		if Isnum(ptr) then begin
			if Isnum(chr) then begin
				lo = min(chr,max=hi)
				wptr = (lo > ptr < hi) - lo
			endif else wptr = ptr > 0
		endif
		case n_elements(wptr) of
			0	:	begin
						fran = '[*,*,0]'
						sran = '[*,*,1]'
					end
			1	:	begin
						dum = strcompress(string(wptr),/rem)
						fran = '[*,'+dum+':*,0]'
						sran = '[*,'+dum+':*,1]'
					end
			2	:	begin
						wptr = wptr(sort(wptr))
						dum = strcompress(strjoin(string(wptr),':'),/rem)
						fran = '[*,'+dum+',0]'
						sran = '[*,'+dum+',1]'
					end
			else:	message, 'Patch range can have at most 2 elements!'
		endcase

		for i = 0, n-1 do begin
			dum = execute(ldnam[i] + acom + slnam[j[i]] + bcom)
			dum = execute(linam[i] + ' = lis')
			dum = execute('zitem = total((' + ldnam[i] + ')'+fran+',2)')
			dum = execute('ezitem = total((' + ldnam[i] + ')'+sran+'^2,2)')
			dum = $
			execute('dthtem = Scan_par_read('+ slnam[j[i]]+ ',"Det_th",/zero)')
			nxs[i] = n_elements(lis)
			tem = fltarr(3,nxs[i])
			tem[0,*] = dthtem
			tem[1,*] = zitem
			tem[2,*] = sqrt(ezitem)
			dum = execute(zinam[i] + ' = tem')
		endfor

		k = Scan_order(zi_0,zi_1,zi_2,zi_3,zi_4,zi_5,zi_6,zi_7)
		nxs = nxs[k]
		for i = 0, n-1 do begin
			dum = execute(oslnam[i] + '=' + slnam[j[k[i]]])
			dum = execute(oldnam[i] + '=' + ldnam[k[i]])
			dum = execute(olinam[i] + '=' + linam[k[i]])
			dum = execute(ozinam[i] + '=' + zinam[k[i]])
			dum = execute('sarr['+si[i]+']=' + oslnam[i])
		endfor

		zcomb = Scan_join(ozi_0,ozi_1,ozi_2,ozi_3,ozi_4,ozi_5,ozi_6,ozi_7, $
		/relaxed, fac = mfac, over = ovl, _extra = _e)
		xsiz = total(nxs-ovl)
		zsiz = (size(old_0))[2]
		ldat = findgen(xsiz,zsiz,2)
		snum = lindgen(xsiz)
		mflis = findgen(xsiz)
		lo = (shift(total([0,nxs] - [ovl,0],/cum),1))[1:*]
		hi = lo + nxs - 1
		slo = string(lo)
		shi = string(hi)
		smfac = string(mfac)
		for i = 0, n-1 do begin
			dum = $
			execute('ldat['+slo[i]+':'+shi[i]+',*,*]='+ smfac[i]+ '*'+oldnam[i])
			dum = execute('snum['+slo[i]+':'+shi[i]+']=' + olinam[i])
			dum = execute('mflis['+slo[i]+':'+shi[i]+'] = mfac['+si[i]+']')
		endfor
		siz = size(ldat)
	endif else begin
		snum = sl_0
		ldat = Scan_ld_read(snum,norm=nrm,chan=chr,bin=bin,_extra=_e)
		siz = size(ldat)
		mflis = replicate(1.,siz[1])
		sarr[0] = sl_0
	endelse

	tit = Default(tit,strcompress(fildat.name + strjoin(' #'+ sarr,',')))

	dist = (Scan_field_read(snum,dnam[0],/const,cfl=cfl))[0]
	if not cfl then message, warn + cwnam[0]
	mpc = (Scan_field_read(snum,dnam[1],/const,cfl=cfl))[0]
	if not cfl then message, warn + cwnam[1]
	ns = (Scan_field_read(snum,dnam[2],/const,cfl=cfl))[0]
	if not cfl then message, warn + cwnam[2]
	lam = (Scan_field_read(snum,dnam[3],/const,tol=1e-3,cfl=cfl))[0]
	if not cfl then message, warn + cwnam[3]
	gl3 = Scan_field_read(snum,dnam[4],/const,tol=1.,cfl=cfl)
	if Arreq(cfl,[1,1,1,1]) then gl3 = gl3[3] else message, warn + cwnam[3]

	horfl = keyword_set(hor)

	spar = Scan_par_read(snum,pnam,/zero)
	talp = !dtor*reform(spar[*,0])
	tbet = !dtor*reform(spar[*,1])
	tdth = !dtor*reform(spar[*,2])
	if horfl then s = sort(tbet) else s = sort(tdth)

	if not Arreq(s,lindgen(siz[1])) then begin
		talp = talp[s]
		tbet = tbet[s]
		tdth = tdth[s]
		ldat = ldat[s,*,*]
	endif

	n = bin*lindgen(siz[2]) + (bin-1)/2. + (Default(chr,0l,/dtyp))[0]
	xyz = mpc*(n - ns)
	wdist = dist + gl3*(1/cos(tbet) - 1)
	alp = (bet = (dth = fltarr(siz[[1,2]])))
	if horfl then begin
		for i = 0l, siz[1] - 1 do begin
			x = xyz/wdist[i]
			alp[i,*] = talp[i]
			bet[i,*] = asin(sin(tbet[i])/sqrt(1+x^2))
			dth[i,*] = tdth[i] + atan(x/cos(tbet[i]))
		endfor
	endif else begin
		for i = 0l, siz[1] - 1 do begin
			z = xyz/wdist[i]
			alp[i,*] = talp[i]
			bet[i,*] = tbet[i] + atan(z)
			dth[i,*] = tdth[i]
		endfor
	endelse

	if keyword_set(ang) then begin
		qxy = !radeg*dth
		qz = !radeg*bet
		znam = 'Beta'
	endif else begin
		k = 2*!pi/lam
		apb = (alp + bet)/2
		amb = (alp - bet)/2
		qxy = 2*k*sqrt((sin(apb)*sin(amb))^2 + cos(alp)*cos(bet)*sin(dth/2)^2)
		if keyword_set(sgn) then qxy = qxy*Sign(dth)
		qz = 2*k*sin(apb)*cos(amb)
		znam = 'Q_z'
	endelse

	if siz[1] gt 1 then begin
		if keyword_set(rel) then tight = 0 else tight = 1
		if horfl then begin
			tldat = fltarr(siz[2],siz[1],2)
			for i = 0, 1 do tldat[*,*,i] = rotate(reform(ldat[*,*,i]),4)
			ldat = tldat
			qxy = rotate(qxy,4)
			qz = rotate(qz,4)
			ind = 2
		endif else ind = 1
		tdat = Img_trig(ldat[*,*,0],qxy,qz,xtr=tqxy,ytr=tqz,min=tight)
		edat = Img_trig(ldat[*,*,1],/last)
		res = fltarr(4,siz[ind],siz[3-ind])
		if horfl then begin
			for i = 0l, siz[1] - 1 do begin
				res[0,*,i] = tqxy
				res[1,*,i] = tqz[i]
			endfor
		endif else begin
			for i = 0l, siz[1] - 1 do begin
				res[0,i,*] = tqxy[i]
				res[1,i,*] = tqz
			endfor
		endelse
		res[2,*,*] = tdat
		res[3,*,*] = edat

		if keyword_set(vrb) and n_elements(wptr) ne 0 then begin
			tem = res[1,0,*]
			vran = [res[1,0,wptr[0]],max(tem)]
			if n_elements(wptr) eq 2 then vran[1] = res[1,0,wptr[1]]
			pvran = strjoin(string(vran,form='(f6.3)'),',')
			print
			print, 'Patching for ' + znam + ' = [' + pvran + ']'
		endif
	endif else begin
		res = fltarr(3,siz[2])
		if horfl then res[0,*] = qxy else res[0,*] = qz
		res[1,*] = ldat[0,*,0]
		res[2,*] = ldat[0,*,1]
	endelse

	return, res
end