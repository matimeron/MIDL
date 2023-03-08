Function Scan_PD_comb_join_old, cfir, csec, $
	patch_range = ptr, reverse = rev, auto = aut, force = frc, relaxed = rlx, $
	factor= fac, efactor= fer, over= ovl, lover= lovl, _extra= _e

;+
; NAME:
;		SCAN_PD_COMB_JOIN
; VERSION:
;		8.2
; PURPOSE:
;		Joins SCAN_PD_COMB type scans, matching amplitude in the overlap areas.
; CATEGORY:
;		SPEC AD data processing.
; CALLING SEQUENCE:
;		Result = SCAN_PD_COMB_JOIN(CFIR, CSEC [, keywords])
; INPUTS:
; 	CFIR
; 		Processed data from SCAN_PD_COMB, a [4,*,*] array.
; 	CSEC
; 		Same type as CFIR.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	PATCH_RANGE
;		The vertical range of pixels to use in patching.
;	/REVERSE													|
;		Switch.  If set, the first scan is interpolated into 	|
;		the second one.  The default is that the second scan	|
;		is interpolated into the first one.						|
;																|	At most one
;		Note:	First and second relates to the scan order as	|	of these two
;				defined by the x-coordinate, not necessarily 	|	keywords may
;				the [CFIR,CSEC] order.							|	be set.
;	/AUTO														|
;		Switch.  If set, REVERSE is set to 1 if the X 			|
;		stepsize of CSEC is smaller than this of CFIR, else		|
;		REVERSE is set to 0.									|
;	/FORCE
;		Switch.  If set, forces junction even when there is no overlap between
;		the scans.  This involves extrapolation which greatly increases
;		uncertainties, so it should be used with care.
;	/RELAXED
;		Switch.  If set, no warning is issued on low overlap unless there is
;		no overlap at all.
;	FACTOR
;		Optional output, see below.
;	EFACTOR
;		Optional output, see below.
;	OVER
;		Optional output, see below.
;	LOVER
;		Optional output, see below.
;	_EXTRA
;		A formal key, used to transfer additional keywords to imbedded routines.
;		Not to be used directly.
; OUTPUTS:
; 		Returns an output in the [4,*,*] format resulting from horizontal
; 		patching of CFIR and CSEC.
; OPTIONAL OUTPUT PARAMETERS:
;	FACTOR
;		Scalar, the multiplicative factor of the match.
;	EFACTOR
;		Scalar, the statistical error of FACTOR.
;	OVER
;		2-element vector, the first value is always 0, the second is the number
;		of x-values in CSEC overlaping the x-range of CFIR.
;	LOVER
;		2-element vector, the first value is the number of x-values in CFIR
;		overlapping the x-range of CSEC, the second is always 0.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		Some overlap between the x-ranges of CFIR and CSEC must exist, unless
; 		/FORCE is set.
; PROCEDURE:
;		Straightforward.  Calls IMG_INT, IMG_LC, SCAN_INTER, SCAN_LC,
;		SCAN_ORDER and SCAN_SORT.  Uses ARREQ, FPU_FIX, INTER_2D, LINFIT_MM, 
;		ONE_OF, SPLIT_XY and TOLER, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-APR-2013 by Mati Meron, as a specialized modification of
;		SCAN_JOIN.
;-

	on_error, 1

	pref = ['No overlap at ','1-point overlap at ']
	post = [' !',', Error estimates unreliable!']
	del = Toler()
	eps = sqrt((machar()).xmin)
	ffl = keyword_set(frc)

	if Arreq([(size(cfir))[0:1],(size(csec))[0:1]],[3,4,3,4]) then begin
		ysiz = min([(size(cfir))[3],(size(csec))[3]],max=mysiz)
		if ysiz lt mysiz then begin
			clo = cfir[*,*,0:ysiz-1]
			chi = csec[*,*,0:ysiz-1]
		endif else begin
			clo = cfir
			chi = csec
		endelse

		if n_elements(ptr) eq 2 then zreg = [min(ptr)> 0, max(ptr)< (ysiz-1)] $
		else zreg = [0,ysiz-1]

		lo = Img_int(clo,/z_int,z_reg=zreg)
		hi = Img_int(chi,/z_int,z_reg=zreg)
		if (Scan_order(lo,hi))[0] eq 0 then begin
			lo = Scan_sort(lo,sor=lsor)
			hi = Scan_sort(hi,sor=hsor)
			clo = clo[*,lsor,*]
			chi = chi[*,hsor,*]
		endif else begin
			llo = Scan_sort(hi,sor=hsor)
			hi = Scan_sort(lo,sor=lsor)
			lo = llo
			cllo = chi[*,hsor,*]
			chi = clo[*,lsor,*]
			clo = cllo
		endelse

		nlo = Split_xy(lo,x=xlo,y=ylo,z=serlo,/keep)
		nhi = Split_xy(hi,x=xhi,y=yhi,z=serhi,/keep)

		case One_of(rev,aut) of
			0	:	rfl = 1
			1	:	rfl = (hi[0,-1]-hi[0,0])/(nhi-1) lt $
						((lo[0,-1]-lo[0,0])/(nlo-1) - del)
			else:	rfl = 0
		endcase

		if rfl then begin
			ovr = where(xhi ge (xlo[0] - del*abs(xlo[0])) and $
			xhi le (xlo[-1]+ del*abs(xlo[-1])),novr,comp=lovr,ncomp=nlovr)
			ovl = [0,novr]
			if novr lt (2 - keyword_set(rlx)) then begin
				mess = pref[novr]
				if novr eq 0 and ffl then ovr = $
				where(xhi eq xhi[0],novr,comp=lovr,ncomp=nlovr)
				if not ffl then begin
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

			mers = ((md[1,*]*serhi[ovr])^2 + (yhi[ovr]*md[2,*])^2)/yhi[ovr]^4
			mer = sqrt(mers > eps)
			fac = (Linfit_mm(md[0,*],md[1,*]/yhi[ovr],1/mer,ord=0,er=fer))[0]
			if novr eq 1 then fer = mer
			fer = fer[0]

			hi = Scan_lc(hi,coef=fac,dcoef=fer)
			ralp = hi[1,ovr]/(hi[2,ovr]^2 > eps)
			rbet = md[1,*]/(md[2,*]^2 > eps)
			alp = ralp/((ralp + rbet) > eps)
			bet = rbet/((ralp + rbet) > eps)
			hi[1,ovr] = (alp*hi[1,ovr] + bet*md[1,*])
			hi[2,ovr] = sqrt((alp*hi[2,ovr])^2 + (bet*md[2,*])^2)
	
			flo = where(xlo lt (xhi[0] - del*abs(xhi[0])),nflo)
			slo = where(xlo gt (xhi[nhi-1]+ del*abs(xhi[nhi-1])),nslo)
			lovl = [nlo - nflo - nslo,0]

			yvec = reform(clo[1,0,*])
			cmd = fltarr(4,novr,ysiz)
			cmd[0:2,*,*] = Inter_2d(xhi[ovr],yvec,/vec,z_sor=clo,/pack,_extra=_e)
			cmd[3,*,*] = Inter_2d(xhi[ovr],yvec,/vec,z_sor=clo,ind=3,_extra=_e)
			chi = Img_lc(chi,coef=fac,dcoef=fer)
			for i = 0, novr-1 do begin
				chi[2,ovr[i],*] = alp[i]*chi[2,ovr[i],*] + bet[i]*cmd[2,i,*]
				chi[3,ovr[i],*] = $
					sqrt((alp[i]*chi[3,ovr[i],*])^2 + (bet[i]*cmd[3,i,*])^2)
			endfor
			res = fltarr(4,nflo+nhi+nslo,ysiz)
			res[*,nflo:nflo+nhi-1,*] = chi
			if nflo gt 0 then res[*,0:nflo-1,*] = clo[*,flo,*]
			if nslo gt 0 then res[*,nflo+nhi:*,*] = clo[*,slo,*]

		endif else begin
			ovr = where(xlo ge (xhi[0] - del*abs(xhi[0])) and $
			xlo le (xhi[-1]+ del*abs(xhi[-1])),novr,comp=lovr,ncomp=nlovr)
			lovl = [novr,0]
			if novr lt (2 - keyword_set(rlx)) then begin
				mess = pref[novr]
				if novr eq 0 and ffl then ovr = $
				where(xlo eq xlo[-1],novr,comp=lovr,ncomp=nlovr)
				if not ffl then begin
					mode = ffl > novr < 1
					mess = mess + string(xlo[-1],form='(f8.3)') + post[mode]
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

			mers = ((md[1,*]*serlo[ovr])^2 + (ylo[ovr]*md[2,*])^2)/md[1,*]^4
			mer = sqrt(mers > eps)
			fac = (Linfit_mm(md[0,*],ylo[ovr]/md[1,*],1/mer,ord=0,er=fer))[0]
			if novr eq 1 then fer = mer
			fer = fer[0]

			hi = Scan_lc(hi,coef=fac,dcoef=fer)
			md = Scan_lc(md,coef=fac,dcoef=fer)
			ralp = lo[1,ovr]/(lo[2,ovr]^2 > eps)
			rbet = md[1,*]/(md[2,*]^2 > eps)
			alp = ralp/((ralp + rbet) > eps)
			bet = rbet/((ralp + rbet) > eps)
			lo[1,ovr] = alp*lo[1,ovr] + bet*md[1,*]
			lo[2,ovr] = sqrt((alp*lo[2,ovr])^2 + (bet*md[2,*])^2)
	
			ehi = where(xhi gt (xlo[nlo-1]+ del*abs(xlo[nlo-1])),nehi)
			ovl = [0,nhi - nehi]

			yvec = reform(chi[1,0,*])
			cmd = fltarr(4,novr,ysiz)
			cmd[0:2,*,*] = Inter_2d(xlo[ovr],yvec,/vec,z_sor=chi,/pack,_extra=_e)
			cmd[3,*,*] = Inter_2d(xlo[ovr],yvec,/vec,z_sor=chi,ind=3,_extra=_e)
			cmd = Img_lc(cmd,coef=fac,dcoef=fer)
			chi = Img_lc(chi,coef=fac,dcoef=fer)
			for i = 0, novr-1 do begin
				clo[2,ovr[i],*] = alp[i]*clo[2,ovr[i],*] + bet[i]*cmd[2,i,*]
				clo[3,ovr[i],*] = $
					sqrt((alp[i]*clo[3,ovr[i],*])^2 + (bet[i]*cmd[3,i,*])^2)
			endfor
			res = fltarr(4,nlo+nehi,ysiz)
			res[*,0:nlo-1,*] = clo
			if nehi gt 0 then res[*,nlo:*,*] = chi[*,ehi,*]
		endelse
	endif else message, 'Bad or missing input!'

	return, FPU_fix(res)
end