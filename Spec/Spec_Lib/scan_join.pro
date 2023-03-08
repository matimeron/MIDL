Function Scan_join, s_0, s_1, s_2, s_3, s_4, s_5, s_6, s_7, $
	s_8, s_9, s_10, s_11, s_12, s_13, s_14, s_15, $
	reverse=rev, force=frc, merge=mrg, external=ext, no_ferr=nof, relaxed=rlx,$
	chi = chi, factor = mfac, efactor = efac, over = ovl, lover = lovl, $
	show = sho, lcolor = lcl, _extra = _e

;+
; NAME:
;		SCAN_JOIN
; VERSION:
;		8.16
; PURPOSE:
;		Joins scans, matching amplitude in the overlap areas.
; CATEGORY:
;		SPEC data processing.
; CALLING SEQUENCE:
;		Result = SCAN_JOIN(S_0, ...  [, keywords])
; INPUTS:
;	S_0, S_1, .. S_15
;		May be:
;			1)  Scans, i.e. [3,n] arrays.
;			2)	Scan numbers, i.e. integers
;			3)  A single array of scan numbers.
;			4)  A single character scalar or array to be processed by RANGE_PROC
;				from MIDL.  See there for allowed expressions.
;
;		In the first 2 cases the  maximal number of scans is 16.  In the third
;		case there is no limit.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/REVERSE
;		Switch.  If set, each scan is interpolated into the following one.  The
;		default is that the following scan is interpolated into the previous one
;	/FORCE
;		Switch.  If set, forces junction even when there is no overlap between
;		some scans.  This involves extrapolation which greatly increases
;		uncertainties, so it should be used with care.
;		
;		Note:	/FORCE is automatically set if EXTERNAL (see below) is provided.
;	/MERGE
;		Switch.  If set, the data in the overlap region is merged instead of
;		being averaged (points with the same x-coordinate are still averaged).
;	/NO_FERR
;		Switch.  If set, fitting errors are ignored in the matching.
;	EXTERNAL
;		Provides an external multiplier to be used in the junction of the scans
;		instead of the (default) internally calculated multiplier(s).  Has to be
;		either of length 1 or of length equal to the number of scans provided.
;		If provided but equal to zero, it is ignored.
;	/RELAXED
;		Switch.  If set, no warning is issued on low overlap unless there is
;		no overlap at all.
;	CHI
;		Optional output, see below.
;	FACTOR
;		Optional output, see below.
;	EFACTOR
;		Optional output, see below.
;	OVER
;		Optional output, see below.
;	LOVER
;		Optional output, see below.
;	/SHOW
;		Switch.  If set, a plot of the patched scan is displayed, with the
;		"low", "hi" and "overlap" parts marked in different colors.
;
;		Note:	If MERGE is set, or the number of scans patched is > 2, SHOW is
;				ignored.
;	LCOLOR
;		Same as in SCAN_SHOW, a list of colors to be used with SHOW.  The
;		default is colors #4, 5 and 6, from !RAINBOW.
;	_EXTRA
;		A formal key, used to transfer additional keywords to SCAN_READ and
;		SCAN_INTER.  Only active when a list is used.
; OUTPUTS:
;		Returns an output in a scan format (i.e. a [3,n] array), where the 3
;		columns (in order) are:  X values, the Y values of the combined scan
;		and the error values of Y (calculated from original error values and
;		the errors of the match parameters.
; OPTIONAL OUTPUT PARAMETERS:
;	CHI
;		A vector of length N (the number of scans present) returning the CHI^2
;		values of the matches.  The i-th value is the CHI^2 of the match between
;		scans # (i-1) and i.  The 0-th value is always 0.
;	FACTOR
;		A vector of length N (the number of scans present) returning the
;		multiplicative factors of the matches.  The i-th value is the factor by
;		which scan #i is multiplied, relative to scan #0.  The 0-th value is
;		always 1.
;	EFACTOR
;		A vector of length N (the number of scans present) returning the errors
;		of the multiplicative factors of the matches.  The i-th value is the
;		statistical error of factor (see above) #i.  The 0-th value is always 0.
;	OVER
;		A vector of length N returning the number of overlap points between
;		consecutive scans.  The i-th value is the number of points in scan #i 
;		overlapping scan #(i-1).  The 0-th value is always 0.
;	LOVER
;		A vector of length N returning the number of overlap points between
;		consecutive scans.  The i-th value is the number of points in scan #i 
;		overlapping scan #(i+1).  The N-th value is always 0.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1)  Each scan must have some overlap with the next one, unless /FORCE
;			is set.
;		2)  Either actual scans or a list of scan numbers may be provided, but
;			not both.
;		3)	If a scan number list is provided, SCAN_READ requires COLUMNS input.
;			See there for details.
; PROCEDURE:
;		Straightforward.  Calls SCAN_INTER, SCAN_LC, SCAN_LIST_VER, SCAN_ORDER,
;		SCAN_PRUNE (when MERGE is set), SCAN_READ (only when in the list mode),
;		SCAN_SCALE and SCAN_SORT.  Uses ARREQ, CAST, FPU_FIX, LINFIT_MM, 
;		SPLIT_XY, TOLER and WHERINSTRUCT from MIDL.
; MODIFICATION HISTORY:
;		Created 15-OCT-2002 by Mati Meron.
;		Modified 5-AUG-2003 by Mati Meron.  Added keyword FACTOR.
;		Modified 15-AUG-2003 by Mati Meron.  Added keyword FORCE.
;		Modified 20-NOV-2003 by Mati Meron.  Internal changes only.
;		Modified 15-APR-2004 by Mati Meron.  Data format change.
;		Modified 25-JUN-2004 by Mati Meron.  Added keyword EXTERNAL.
;		Modified 20-FEB-2006 by Mati Meron.  Added keyword OVERLAP.
;		Modified 5-APR-2006 by Mati Meron.  Internal changes.
;		Modified 30-MAR-2008 by Mati Meron.  Added keyword EFACTOR.
;		Modified 25-OCT-2008 by Mati Meron.  Added keyword REVERSE.
;		Modified 10-DEC-2008 by Mati Meron.  Internal changes.
;		Modified 1-AUG-2009 by Mati Meron.  Internal changes.
;		Modified 15-AUG-2009 by Mati Meron.  Increased maximal number of scans
;		from 8 to 16.  Added keyword LOVER.
;		Modified 25-APR-2010 by Mati Meron.  Added keyword MERGE.
;		Modified 20-FEB-2010 by Mati Meron.  Internal changes.
;		Modified 20-JUN-2011 by Mati Meron.  Internal changes.
;		Modified 25-AUG-2011 by Mati Meron.  Bug fix.
;		Modified 10-DEC-2011 by Mati Meron.  Bug fix.
;		Modified 10-JUL-2012 by Mati Meron.  Bug fix.
;		MOdified 4-APR-2016 by Mati Meron.  Added keywords SHOW and LCOLOR.
;-

	on_error, 1
	snams =  strcompress('s_' + sindgen(16),/remove)
	pref = ['No overlap at ','1-point overlap at ']
	post = [' !',', Error estimates unreliable!']
	del = Toler()
	eps = sqrt((machar()).xmin)
	rfl = keyword_set(rev)
	ffl = keyword_set(frc)
	mfl = keyword_set(mrg)

	nsc = Scan_list_ver(s_0,s_1,s_2,s_3,s_4,s_5,s_6,s_7, $
	s_8, s_9, s_10, s_11, s_12, s_13, s_14, s_15, flag=lfl,lis=slis)
	if nsc eq 0 then message, 'Missing or inconsistent input!'
	shofl = keyword_set(sho) and nsc eq 2 and not mfl
	if shofl then lcl = Default(lcl,!rainbow[4:6])

	nex = n_elements(ext)
	if nex gt 0 then begin
		if not Arreq(ext,0*ext) then begin
			if nex eq 1 then wext = Cast(replicate(ext,nsc),4) $
			else if nex eq nsc then wext = Cast(ext,4) $
			else message, 'Wrong number of external factors!'
			efl = 1
			ffl = 1
		endif else efl = 0
	endif else efl = 0

	efac = (chi = fltarr(nsc))
	mfac = efac + 1
	ovl = (lovl = lonarr(nsc))
	if lfl then begin
		res = Scan_read(slis[0],_extra=_e)
		dum = (Wherinstruct('new',_e))[0]
		if dum ge 0 then _e.(dum) = 0
	endif else res = s_0

	for i = 1, nsc - 1 do begin
		fir = res
		if lfl then sec = Scan_read(slis[i],_extra=_e) $
		else idum = execute('sec = ' + snams[i])

		if (Scan_order(fir,sec))[0] eq 0 then begin
			lo = Scan_sort(fir)
			hi = Scan_sort(sec)
		endif else begin
			lo = Scan_sort(sec)
			hi = Scan_sort(fir)
		endelse

		nlo = Split_xy(lo,x=xlo,y=ylo,z=serlo,/keep)
		nhi = Split_xy(hi,x=xhi,y=yhi,z=serhi,/keep)

		if keyword_set(rev) then begin
			ovr = where(xhi ge (xlo[0] - del*abs(xlo[0])) and $
			xhi le (xlo[nlo-1]+ del*abs(xlo[nlo-1])),novr,comp=lovr,ncomp=nlovr)
			ovl[i] = novr
			if novr lt (2 - keyword_set(rlx)) then begin
				mess = pref[novr]
				if novr eq 0 and ffl then ovr = $
				where(xhi eq xhi[0],novr,comp=lovr,ncomp=nlovr)
				if not (ffl and mfl) then begin
					mode = ffl > novr < 1
					mess = mess + string(xhi[0],form='(f8.3)') + post[mode]
					message, '	' + strcompress(mess), con = mode, inf = mode
				endif
			endif

			md = Scan_inter(lo,xhi[ovr],_extra=_e)
			noz = where(md[1,*] ne 0,nnoz,comp=zer,ncomp=nzer)
			if nzer gt 0 then begin
				if nnoz gt 0 then begin
					ovr = ovr[noz]
					novr = nnoz
					md = md[*,noz]
					if nnoz eq 1 then md = reform(md,3,1)
				endif else message, 'Unpatcheable data!'
			endif

			if efl then begin
				fac = wext[i]
				fer = 0.
			endif else begin
				mers= ((md[1,*]*serhi[ovr])^2 + (yhi[ovr]*md[2,*])^2)/yhi[ovr]^4
				mer = sqrt(mers > eps)
				fac = $
				Linfit_mm(md[0,*],md[1,*]/yhi[ovr],1/mer,ord=0,res=chv,er=fer)
				if novr eq 1 then fer = mer
				if keyword_set(nof) then fer = 0.
				chi[i] = chv
			endelse
			mfac[i] = fac[0]
			efac[i] = fer[0]

			hi = Scan_lc(hi,coef=fac[0],dcoef=fer[0])
			if mfl then begin
				res = [[lo],[hi]]
				res = res[*,sort(res[0,*])]
			endif else begin
				ralp = hi[1,ovr]/(hi[2,ovr]^2 > eps)
				rbet = md[1,*]/(md[2,*]^2 > eps)
				alp = ralp/((ralp + rbet) > eps)
				bet = rbet/((ralp + rbet) > eps)
				hi[1,ovr] = (alp*hi[1,ovr] + bet*md[1,*])
				hi[2,ovr] = sqrt((alp*hi[2,ovr])^2 + (bet*md[2,*])^2)
	
				flo = where(xlo lt (xhi[0] - del*abs(xhi[0])),nflo)
				slo = where(xlo gt (xhi[nhi-1]+ del*abs(xhi[nhi-1])),nslo)
				lovl[i-1] = nlo - nflo - nslo
				res = fltarr(3,nflo+nhi+nslo)
				res[*,nflo:nflo+nhi-1] = hi
				if nflo gt 0 then res[*,0:nflo-1] = lo[*,flo]
				if nslo gt 0 then res[*,nflo+nhi:*] = lo[*,slo]

				if shofl then begin
					olo = min(ovr,max=ohi)
					tri = res[*,nflo+olo:nflo+ohi]
					if nhi gt (ohi+1) then two = res[*,nflo+ohi:nflo+nhi-1] $
					else two = tri
					if nflo gt 0 then fone = res[*,0:nflo+olo] else fone = tri
					if nslo gt 0 then sone = res[*,nflo+ohi:nflo+nhi+nslo-1] $
					else sone = tri
					Scan_show, fone,sone,two,tri, lcol=lcl[[0,0,1,2]], _extra=_e
				endif
			endelse

		endif else begin
			ovr = where(xlo ge (xhi[0] - del*abs(xhi[0])) and $
			xlo le (xhi[nhi-1]+ del*abs(xhi[nhi-1])),novr,comp=lovr,ncomp=nlovr)
			lovl[i-1] = novr
			if novr lt (2 - keyword_set(rlx)) then begin
				mess = pref[novr]
				if novr eq 0 and ffl then ovr = $
				where(xlo eq xlo[nlo-1],novr,comp=lovr,ncomp=nlovr)
				if not (ffl and mfl) then begin
					mode = ffl > novr < 1
					mess = mess + string(xlo[nlo-1],form='(f8.3)') + post[mode]
					message, '	' + strcompress(mess), con = mode, inf = mode
				endif
			endif

			md = Scan_inter(hi,xlo[ovr],_extra=_e)
			noz = where(md[1,*] ne 0,nnoz,comp=zer,ncomp=nzer)
			if nzer gt 0 then begin
				if nnoz gt 0 then begin
					ovr = ovr[noz]
					novr = nnoz
					md = md[*,noz]
					if nnoz eq 1 then md = reform(md,3,1)
				endif else message, 'Unpatcheable data!'
			endif

			if efl then begin
				fac = wext[i]
				fer = 0.
			endif else begin
				mers = ((md[1,*]*serlo[ovr])^2 + (ylo[ovr]*md[2,*])^2)/md[1,*]^4
				mer = sqrt(mers > eps)
				fac = $
				Linfit_mm(md[0,*],ylo[ovr]/md[1,*],1/mer,ord=0,res=chv,er=fer)
				if novr eq 1 then fer = mer
				if keyword_set(nof) then fer = 0.
				chi[i] = chv
			endelse
			mfac[i] = fac[0]
			efac[i] = fer[0]

			hi = Scan_lc(hi,coef=fac[0],dcoef=fer[0])
			if mfl then begin
				res = [[lo],[hi]]
				res = res[*,sort(res[0,*])]
			endif else begin	
				md = Scan_lc(md,coef=fac[0],dcoef=fer[0])
				ralp = lo[1,ovr]/(lo[2,ovr]^2 > eps)
				rbet = md[1,*]/(md[2,*]^2 > eps)
				alp = ralp/((ralp + rbet) > eps)
				bet = rbet/((ralp + rbet) > eps)
				lo[1,ovr] = alp*lo[1,ovr] + bet*md[1,*]
				lo[2,ovr] = sqrt((alp*lo[2,ovr])^2 + (bet*md[2,*])^2)
	
				ehi = where(xhi gt (xlo[nlo-1]+ del*abs(xlo[nlo-1])),nehi)
				ovl[i] = nhi - nehi
				res = fltarr(3,nlo+nehi)
				res[*,0:nlo-1] = lo
				if nehi gt 0 then res[*,nlo:*] = hi[*,ehi]

				if shofl then begin
					olo = min(ovr,max=ohi)
					tri = res[*,olo:ohi]
					if nehi gt 0 then two = res[*,ohi:nlo+nehi-1] else two = tri
					if olo gt 0 then one = res[*,0:olo] else one = tri
					Scan_show, one, two, tri, lcol = lcl, _extra=_e
				endif
			endelse
		endelse
	endfor

	if mfl then begin
		step = (max(res[0,*],min=min) - min)/((size(res))[2] - 1)
		res = Scan_prune(res,1e-2*step)
	endif

	return, FPU_fix(res)
end