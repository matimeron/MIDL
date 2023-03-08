Function ID_field_old, rat, smco = smc, enhanced = enh, inverse = inv, $
	special = spc, quiet = qui, range = ran

;+
; NAME:
;		ID_FIELD
; VERSION:
;		8.213
; PURPOSE:
;		Calculates the field of an insertion device
; CATEGORY:
;		SR specific
; CALLING SEQUENCE:
;		Result = ID_FIELD( RAT [, keywords])
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
;	/INVERSE
;		Switch.  If set, an inverse calculation is performed, i.e.  the input
;		is assumed to be the magnetic field (in Tesla) and the corresponding
;		gap/period ratio is returned.
;	/SPECIAL
;		Switch.  If set, and the switch /INVERSE is also set, the return
;		value(s) is gap/period divided by the field.  For internal use only.
;	/QUIET
;		Switch.  If set, warning messages (more about it below) are supressed.
;	RANGE
;		Optional output, see below.
; OUTPUTS:
;		Returns either field calculated from gap/period ratio or, when /INVERSE
;		is set, the ratio calculated from the field.  Output format is same as
;		input (however, note the keyword /SPECIAL).  For input values beyond
;		the validity range of the approximation, NaN is returned.  In such case
;		a warning message is printed to the screen unless /QUIET is set.
; OPTIONAL OUTPUT PARAMETERS:
;	RANGE
;		Returns a 2-element vector containing the limits for RAT or, if /INVERSE
;		is set, for the field.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		For forward operation, the values of RAT should be 0<= rat <1.65, though
;		already for RAT > 0.7 the approximation gets progressively worse.  For
;		inverse operation, the value of the field must be between 0.06 and 3.9
;		Tesla (and really should be between 0.2 and 2).  For out of range values
;		NaN is returned.
; PROCEDURE:
;		Follows the Halbach formula approximation (see ANL-88-9), as updated by
;		Roger Dejus in the APS LS-note 314.  Calls FPU_FIX, POLEVAL and TOLER 
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2005 by Mati Meron.
;		Updated 20-OCT-2013 by Mati Meron.  Updated coefficients and added
;		keyword SMCO.
;		Updated 30-OCT-2013 by Mati Meron.  Replaced LS-314 coefficients with
;		coefficients calculated using a fit to Roger Dejus' values.
;-

	on_error, 1

	if keyword_set(smc) then coef = [alog(2.940),-4.62,1.37] $
	else coef = [1.03700,-3.82800,0.678800,-0.212000]
;	else coef = [alog(3.276),-4.51,1.20]
	corr = Toler()*[1,-1]

	res = 0.*rat
	if keyword_set(inv) then begin
		ran = [coef[1]^2/(4*coef[2]),0.]
		tem = coef[0] - alog(rat)
		good = where(tem ge ran[1] and tem lt ran[0],ngood,comp=bad,ncomp=nbad)
		if ngood gt 0 then begin
			res[good] = $
			2*tem[good]/(sqrt(coef[1]^2 - 4*coef[2]*tem[good]) - coef[1])
			if keyword_set(spc) then res[good] = res[good]/rat[good]
		endif
		ran = exp(coef[0] - ran)
	endif else begin
		ran = [0.,(-coef[1]/(2*coef[2]))]
		good = where(rat ge ran[0] and rat lt ran[1],ngood,comp=bad,ncomp=nbad)
		if ngood gt 0 then res[good] = exp(Poleval(rat[good],coef))
	endelse
	if nbad gt 0 then begin
		res[bad] = !values.f_nan
		if not keyword_set(qui) then message, 'Some out of range values!',/con
	endif
	ran = ran + corr

	return, FPU_fix(res)
end