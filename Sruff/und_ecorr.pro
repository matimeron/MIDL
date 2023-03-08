Function UND_ecorr, ene, emx, harmonic= har, nper=npr, period=per,scu=scu, $
	def = def, factor = fac, _extra = _e

;+
; NAME:
;		UND_ECORR
; VERSION:
;		8.45
; PURPOSE:
;		Evaluates the nominal undulator energy needed to yield maximal flux at
;		the given energy.
; CATEGORY:
;		Synchrotron calculations.
; CALLING SEQUENCE:
;		Result = UND_ECORR( ENE, keywords)
; INPUTS:
;	ENE
;		Beam energy in KeV, scalar.
;	EMX
;		Scalar, maximal energy for the given harmonic.  Optional, if not
;		provided will be evaluated internally.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	HARMONIC
;		Integer type scalar, the harmonic number.  Mandatory.
;	NPER
;		Integer type scalar, the number of undulator periods.  Mandatory.
;	PERIOD
;		The length of the undulator period.  Optional, if given passed to
;		UND_EFACTOR.
;	/SCU
;		Switch.  If set, the evaluation is for a superconducting undulator.
;	/DEF
;		Switch.  Set by default, causes the values of some missing parameters to
;		be read from !BLPAR.  If explicitly set to zero, additional parameters
;		need to be provided.  See UND_EFACTOR for full list.
;	FACTOR
;		Optional output, see below.
;	_EXTRA
;		A formal keyword for transfering keywords to embedded routines.  Not to
;		be used directly.
; OUTPUTS:
;		Returns the undulator nominal energy (for the given harmonic) required
;		to yield maximal flux.
; OPTIONAL OUTPUT PARAMETERS:
; 	ERROR
; 		Returns the estimated error of the result.
; 	FACTOR
; 		Returns the value of the correction factor to be applied to the flux
; 		calculated with UND_FLUX, for undulator set to the output result and
; 		measurement energy at ENE.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		ENE must be nonnegative and within the range allowed for the given
;		harmonic.
; PROCEDURE:
;		Iterated calculation using UND_EFACTOR, see details there.  Calls
;		BL_DEFAULTS and ID_CONV.  Also calls CAST, DEFAULT, ISNUM, MAKE_GRID,
;		SEQLIM, SPLIN_COEFFS and SPLINROOT, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-FEB-2013 by Mati Meron.
;		Modified 1-JUL-2013 by Mati Meron.  Added keyword FACTOR.
;		Modified 25-OCT-2014 by Mati Meron.  Internal changes.
;		Modified 25-OCT-2015 by Mati Meron.  Added keyword SCU for
;		superconducting undulator option.
;		Modified 10-NOV-2015 by Mati Meron.  Significant rewrite.  Added
;		input EMX and keyword PERIOD.
;		Modified 15-JAN-2016 by Mati Meron.  Slight internal changes.
;		Modified 20-DEC-2020 by Mati Meron.  Internal changes.
;-

	on_error, 1
	mult = 1d3
	niter = 2
	nspl = 10

	wdef = Default(def,1)
	if Isnum(emx) eq 0 then begin
		if not Isnum(per) then begin
			if wdef then begin
				BL_Defaults, dev=udl, scu=scu
				wper = floor(2*mult*udl/npr)/2d
			endif else message, 'Missing EMX, PER, DEF, cannot proceed!'
		endif else if per lt 1 then wper = mult*per else wper = 1d*per
		ID_conv, per=wper, k=1d, emax=emx, scu=scu, _extra=_e
	endif else emx = Cast(emx,5)

	elim = har*emx
	edel = emx/npr
	ranlim = (elim-ene)
	if ranlim ge 5*edel/3 then begin
		eran = [0,edel] < ranlim
		iter = 0
		repeat begin
			ehr = Make_grid(ene + eran,nspl)
			fac = UND_efactor($
			ene,ehr,emx,harm=har,nper=npr,per=per,scu=scu,def=wdef,_extra=_e)
			sp = Splin_coeffs(ehr,fac)
			res = (Splinroot(sp,1e-6,/der,sta=sta))[0]
			if not sta then begin
				max = max(fac,mloc)
				if iter eq 0 then begin
					if mloc eq 0 then eran = eran/2 else eran = 2*eran < ranlim
				endif else res = ene + eran[mloc ne 0]
				iter = iter + 1
			endif
		endrep until sta or (iter eq niter)
		fac= UND_efactor(ene,res,emx,$
		harm=har,nper=npr,per=per,scu=scu,def=wdef,scale=0,_extra=_e)
	endif else begin
		res = ene
		fac = 1
	endelse

	return, res
end