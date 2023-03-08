Function Fprime, ene, element = ele, edge = edg, hwid = hwi, _extra = _e

;+
; NAME:
;		FPRIME
; VERSION:
;		8.492
; PURPOSE:
;		Calculates the F' part of the forward atomic scattering factor
; CATEGORY:
;		X-ray calculations.
; CALLING SEQUENCE:
;		Result = FPRIME (ENE, ELEMENT = ELE [, optional keywords])
; INPUTS:
;	ENE
;		Energy, assumed in the units specified by the variable ENUN in the
;		common block SXR_STUFF, unless specified otherwise by the keyword ENUNS.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ELEMENT
;		Accepts a character value (chemical symbol) or a numeric value (Z-value)
;		representing a single element.  Multiple elements are not allowed.
;	EDGE
;		A character value representing an absorption edge (e.g. "K", "L1",..)
;		If prowided, the next keyword parameter, HWID, must also be provided.
;	HWID
;		Numeric scalar, the half-width of the absorption edge given by EDGE.
;		Providing EDGE and HWID forces a finite-width evaluation of F' in the
;		vicinity of the edge.
;	_EXTRA
;		A formal keyword used to pass keywords to the function DISPER_FUN.  Not
;		to be used directly.  See comment in RESTRICTIONS.
; OUTPUTS:
;		Returns the values of F' for all the energies in EN.  Output form is
;		same as EN.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		SXR_STUFF.  See LOAD_ABS_COEFFS for more information.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The element specified must be listed in ABCTAB (i.e. no transuranics).
;		EDGE, if given, must be an existing edge for the element specified.
; PROCEDURE:
;		Evaluation using home derived approximations (details elsewhere).
;		Uses calls to ELECOMP, DISPER_FUN, EDISPER_FUN and LOAD_ABS_COEFFS.
;		Also calls FPU_FIX, STRMATCH_MM and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 15-SEP-2017 by Mati Meron, through "surgery" on DIELECT.
;-

	common sxr_stuff, sorlist, abctab, enun, denun, curbeam

	on_error, 1
	Load_abs_coeffs

	pfac = float(1d-4/(!dpi*!srcon.re*!srcon.hc*!srcon.scal*!pcon.na))
	edlis = ['','K ','L1','L2','L3','M1','M2','M3','M4','M5',$
		'N1','N2','N3','N4','N5','N6','N7']

	z = Elecomp(ele, number = nel)
	if nel eq 1 then begin
		if Type(edg) eq 7 then begin
			edl = Strmatch_mm(edg,edlis,strlen(edg))
			if edl le abctab[z].edlen then edl = abctab[z].edlen-edl+1 $
			else edl = -1
		endif else edl = -1

		sfac = pfac*abctab[z].a
		ned = abctab[z].edlen
		jmp = abctab[z].edtab[0:ned,0]*abctab[z].edtab[0:ned,1]
		for l = 0l, ned do begin
			q = abctab[z].edtab[l,2]
			t = abctab[z].edtab[l,0]/ene
			if l gt 0 then begin
				dsf = Disper_fun(t,q,_extra=_e)
				dum = where(dsf ne 0,ndum)
				if ndum ne 0 then begin
					pres[dum] = pres[dum] + jmp[l]*t[dum]^(1+q)*dsf[dum]
					if l eq edl then pres[dum] = pres[dum] + $
					jmp[l]*t[dum]*Edisper_fun(t[dum],hwi/ene,_extra=_e)
				endif
			endif else pres = jmp[0]*t^(1+q)*Disper_fun(0,q,_extra=_e)
		endfor
		res = sfac*pres
	endif else message, 'Only single elements allowed!'

	return, FPU_fix(res)
end