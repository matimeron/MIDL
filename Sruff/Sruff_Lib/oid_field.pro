Function OID_field, rat, smco = smc, enhanced = enh, scu = scu, gap = gap, $
	inverse = inv, special = spc, quiet = qui, range = ran, _extra = _e

;+
; NAME:
;		OID_FIELD
; VERSION:
;		8.44
; PURPOSE:
;		Calculates the field of an insertion device
; CATEGORY:
;		SR specific
; CALLING SEQUENCE:
;		Result = OID_FIELD( RAT [, keywords])
; INPUTS:
;	RAT
;		Numeric scalar or vector.  The ratio gap/period, unless /INVERSE (see
;		below) is set.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 	/SMCO
; 		Switch.  If set, the calculation is for SmCo magnets.  The default
; 		calculation is for NdFeB magnets.
;	/ENHANCED
;		Switch, obsolete.  Serves no function in this version.
;	/SCU
;		Switch.  If set, the calculation is for superconducting (NbTi) magnets.
;	GAP
;		Scalar, the gap value, in mm, for the superconducting magnets.  If SCU
;		is set and GAP is not provided, it defaults to the value in !BLPAR.SGAP.
;		If SCU is not set, GAP has no effect.
;	/INVERSE
;		Switch.  If set, an inverse calculation is performed, i.e. the input
;		is assumed to be the magnetic field (in Tesla) and the corresponding
;		gap/period ratio is returned.
;	/SPECIAL
;		Switch.  If set, and the switch /INVERSE is also set, the return
;		value(s) is gap/period divided by the field.  For internal use only.
;	/QUIET
;		Switch.  If set, warning messages (more about it below) are supressed.
;	RANGE
;		Optional output, see below.
;	_EXTRA
;		A formal keyword used to accept keywords passed indirectly.  Not to be
;		used directly.
; OUTPUTS:
;		Returns either field calculated from gap/period ratio or, when /INVERSE
;		is set, the ratio calculated from the field.  Output format is same as
;		input (however, note the keyword /SPECIAL).  For input values beyond
;		the validity range of the approximation, NaN is returned.  In such case
;		a warning message is printed to the screen unless /QUIET is set.
; OPTIONAL OUTPUT PARAMETERS:
;	RANGE
;		Returns a 2-element vector containing the limits for RAT or, if /INVERSE
;		is set, for the field.  For superconducting magnets there are no
;		currently establised limits, and RANGE returns rather arbitrary values.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
; 		For NdFeB magnets (default) the restrictions are:
; 			Forward operation	-		0	 < 	RAT	< 2.453
; 			Inverse operation	-	0.000612 < 	B	< 2.82
; 		For the older SmCo magnets the corresponding restrictions are:
; 			Forward operation	-		0	 < 	RAT	< 1.686
; 			Inverse operation	-	0.0599 	 < 	B	< 2.94
;
;		In practice the approximations start losing reliability for field values
;		ot of the [0.2, 2] Tesla range.
;
;		For Superconducting magnets limits have not been established (here) yet.
; PROCEDURE:
;		Follows the Halbach formula approximation (see ANL-88-9), as updated by
;		Roger Dejus in the APS LS-note 314.  For superconducting magnets follows
;		a fit to data provided by Roger Dejus on 10/18/15.
;		Calls OSCU_FIELD.  Calls ABS_MM, DEFAULT, FPU_FIX, POLEVAL, POLSOLVE,
;		REAL_MM, ROOT and TOLER from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2005 by Mati Meron.
;		Updated 20-OCT-2013 by Mati Meron.  Updated coefficients and added
;		keyword SMCO.
;		Updated 30-OCT-2013 by Mati Meron.  Replaced LS-314 coefficients with
;		coefficients calculated using a fit to Roger Dejus' values.
;		Significant update 20-OCT-2015 by Mati Meron.  Corrected inaccuraces in
;		inverse operation.  Added Superconducting magnets, with the SCU and GAP
;		keywords.
;		Obsoleted and renamed OID_FIELD 15-JUL-2020 by Mati Meron.
;-

	on_error, 1

	if not keyword_set(scu) then begin
		if keyword_set(smc) then coef = [alog(2.940),-4.62,1.37] $
		else coef = [1.03700,-3.82800,0.678800,-0.212000]
	endif else wgap = Default(gap,!blpar.sgap,/dtyp)

	res = 0.*rat
	if keyword_set(inv) then begin
		if keyword_set(scu) then begin
			ran = [0.01,10]
			for i = 0, n_elements(rat) - 1 do $
			res[i] = Root('oscu_field',ran,par=wgap,field=rat[i])
			if keyword_set(spc) then res = res/rat
			nbad = 0
		endif else begin
			root = (Polsolve(coef[1:*]*(1+findgen(n_elements(coef)-1))))[0]
			ran = [Poleval(Abs_mm(root),coef),coef[0]]
			lob = alog(rat)
			good = $
			where(lob ge ran[0] and lob le ran[1],ngood,comp=bad,ncomp=nbad)
			if ngood gt 0 then begin
				ccoef = coef[0] - lob[good]
				for i = 0, ngood-1 do res[good[i]] = $
				Real_mm((Polsolve([ccoef[i],coef[1:*]],/alt))[0])
				if keyword_set(spc) then res[good] = res[good]/rat[good]
			endif
			ran = exp(ran*[1-Toler(),1])
		endelse
	endif else begin
		if keyword_set(scu) then begin
			res = OSCU_field(rat,wgap)
			nbad = 0
			ran = [Toler(),1/Toler()]
		endif else begin
			root = (Polsolve(coef[1:*]*(1+findgen(n_elements(coef)-1))))[0]
			ran = [0.,Abs_mm(root)]
			good = $
			where(rat ge ran[0] and rat le ran[1],ngood,comp=bad,ncomp=nbad)
			if ngood gt 0 then res[good] = exp(Poleval(rat[good],coef))
			ran = ran*[1,1-Toler()]
		endelse
	endelse
	if nbad gt 0 then begin
		res[bad] = !values.f_nan
		if not keyword_set(qui) then message, 'Some out of range values!',/con
	endif

	return, FPU_fix(res)
end