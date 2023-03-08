Function OSCU_field, rat, gap, field = fld

;+
; NAME:
;		OSCU_FIELD
; VERSION:
;		8.44
; PURPOSE:
;		Undulator magnetic field calculation for superconducting (NbTi) magnets.
; CATEGORY:
;		Mathematical, SR specific
; CALLING SEQUENCE:
;		Result = OSCU_FIELD( RAT, GAP [, FIELD = FLD])
; INPUTS:
;	RAT
;		Numerical scalar or vector.  The ratio gap/period.
;	GAP
;		Numerical scalar (if given as a vector, only GAP[0] is used).  The
;		undulator gap value, in mm.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FIELD
;		Numerical scalar of vector, magnetic field value.  If given, it is
;		subtracted from the calculation result.  For internal purposes.
; OUTPUTS:
;		Returns calculation result, details depend on case.
; OPTIONAL OUTPUT PARAMETERS:
;		None
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straighforward.  Uses an approximation to the field values provided by
;		Roger Dejus on 10/18/15.  Calls DEFAULT and FPU_FIX from MIDL.
; MODIFICATION HISTORY:
;		Created 25-OCT-2015 by Mati Meron.
;		Obsoleted and renamed OSCU_FIELD 15-JUL-2020 by Mati Meron.
;-

	on_error, 1

	coe = 1.196
	res = coe*sqrt(gap[0]/rat)*exp(-!pi*rat) - Default(fld,0.,/dtyp)

	return, FPU_fix(res)
end