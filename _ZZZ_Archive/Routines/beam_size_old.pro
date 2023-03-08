Function Beam_size_old, energy= ene, rsig= rsg, asig= asg, undlen= udl, dist= dst, $
	horizontal = hor, vertical = ver, fwhm = fwhm, ang_size = asz

;+
; NAME:
;		BEAM_SIZE
; VERSION:
;		8.15
; PURPOSE:
;		Evaluates a synchrotron beam size.
; CATEGORY:
;		Synchrotron calculations, local beamline specific.
; CALLING SEQUENCE:
;		Result = BEAM_SIZE( keywords)
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	ENERGY
;		Scalar, Beam energy in KeV.  Default is 10keV.
;	RSIG
;		Beam spatial sigma, in mm.  Can be given as a 2-element vector, in
;		[horizontal, vertical] order or as a scalar.  If not given, default
;		values are read from the global parameter !BLPAR.
;	ASIG
;		Same as RSIG, for angular sigma, value(s) in mr.
;	UNDLEN
;		Undulator length, in m.  If not given, default value is read from !BLPAR
;	DIST
;		Distance from source, in m.  If not given, it is taken as 0.
;	/HORIZONTAL											|
;		Switch.  If set, only the horizontal dimension 	|	These two keywords
;		values are returned.							|	are mutually
;	/VERTICAL											|	exclusive.
;		Switch.  If set, only the horizontal dimension 	|
;		values are returned.							|
;	/FWHM
;		Switch.  If set, FWHM values are returned.  Default is to return sigma
;		values.
;	ANG_SIZE
;		Optional output, see below.
; OUTPUTS:
;		Returns the beam size (sigma or fwhm) corresponding to the provided 
;		parameters.  If one of the keywords HORIZONTAL or VERTICAL is set, the
;		corresponding value is returned, else both values are returned.  
; OPTIONAL OUTPUT PARAMETERS:
;	ANG_SIZE
;		Returns the angular beam size, same stipulations as the standard output.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward calculation, including radiative broadening.
;		Calls DEFAULT, ISNUM and ONE_OF from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JAN-2012 by Mati Meron.
;-

	on_error, 1

	if n_elements(ene) gt 1 then message, 'Energy must be a scalar!'
	lam = float(1e-10*!srcon.conv/Default(ene[0],10.,/dtyp))
	rsg = Default(rsg,!blpar.rsig)
	asg = Default(asg,!blpar.asig)
	nxy = n_elements(rsg)
	if nxy gt 2 or nxy ne n_elements(asg) then message, $
	'RSIG and ASIG have to have either 1 or two elements, both!'
	udl = Default(udl,!blpar.dlen)

	ursg = 1e3*sqrt(2*lam*udl)/(4*!pi)
	uasg = 1e3*sqrt(lam/(2*udl))

	rsz = sqrt(rsg^2 + ursg^2)
	asz = sqrt(asg^2 + uasg^2)
	if Isnum(dst) then rsz = sqrt(rsz^2 + dst^2*asz^2)

	whi = One_of(hor,ver)
	if whi ge 0 and nxy eq 2 then begin
		rsz = rsz[whi]
		asz = asz[whi]
	endif

	if keyword_set(fwhm) then begin
		mult = sqrt(alog(256))
		rsz = mult*rsz
		asz = mult*asz
	endif

	return, rsz
end