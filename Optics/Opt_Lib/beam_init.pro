Function Beam_init, wavl, x_source = xsor, y_source = ysor, internal = int, $
	sigma = sig, add_cp = acp, und_length = udl, $
	name = nam, comment = com, sorname = snm

;+
; NAME:
;		BEAM_INIT
; VERSION:
;		5.6
; PURPOSE:
;		Generates and initializes a beam structure of type OPBEAM.
; CATEGORY:
;		Optical calculations.
; CALLING SEQUENCE:
;		Result = BEAM_INIT( WAVL, keywords)
; INPUTS:
;	WAVL
;		Wavelength, in Angstrem (default) or meters (if /INTERNAL is set).
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	X_SOURCE
;		2-element vector containing, in order, the source linear and angular
;		sizes in the X dimension in (micron,microradian) units unless /INTERNAL
;		is set.  Assumed full sizes (i.e. sqrt(4*pi)*sigma) unless SIGMA is set.
;	Y_SOURCE
;		Same as X_SOURCE, in the Y dimension.
;
;		Note:  At least one of X_SOURCE and Y_SOURCE must be given.
;	/INTERNAL
;		Switch.  Selects between internal (meter, radian) and external
;		(micron, microradian) units.
;	/SIGMA
;		Switch.  If set, the input values of X_SOURCE and Y_SOURCE are assumed
;		to be sigma-values.
;	/ADD_CP
;		Switch.  If set, the angular sizes are adjusted to include the coherent
;		contribution.
;	UND_LENGTH
;		Value of undulator length (meter).  If given, radiative corrections
;		are added to the linear and angular sizes.
;
;		Note:  ADD_CP and UND_LENGTH cannot be both specified.
;
;	NAME
;		Character input, optional, the name of the beam.
;	COMMENT
;		Character input, optional, informational comment.
;	SORNAME
;		Character input, optional, the name of the source.
; OUTPUTS:
;		Returns a structure of type OPBEAM, with the fields filled up according
;		to the input values.  Only the first row (source) in the array fields
;		is filled.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block BEAM_STUFF, as defined in INIT_OPT.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than those listed above.
; PROCEDURE:
;		Straightforward.  Calls FPU_FIX, HOW_MANY, ISNUM, ONE_OF and TYPE from
;		MIDL.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron as INIT_BEAM
;		Modified 30-MAY-2002 by Mati Meron.
;		Modified 5-DEC-2006 by Mati Meron.  Internal changes and changed name
;		to BEAM_INIT, for consistency.
;-

	common beam_stuff, exs, npomax, mmicr, mwlen
	on_error, 1

	beam = {opbeam}
	if keyword_set(sig) then smult = 4*!pi else smult = 1.
	if not keyword_set(int) then begin
		wavfac = mwlen
		sizfac = mmicr
	endif else wavfac = (sizfac = 1)

	if How_many(fir=xsor,sec=ysor,whi=whi) gt 0 and Isnum(wavl) then begin
		if Type(nam) eq 7 then beam.name = nam
		if Type(com) eq 7 then beam.comment = com
		if Type(snm) eq 7 then beam.elname[0] = snm
		beam.set = whi
		beam.npoints = 1
		beam.wavl = wavfac*wavl
		wavlsq = beam.wavl^2
	endif else message, 'Insufficient input!'

	what = One_of(acp,udl) + 1
	if what eq 2 then clen = udl/(2*!pi)

	if (whi and 1) ne 0 then begin
		beam.xsec[0].optel = 'psr'
		wsorsq = smult*(sizfac*xsor)^2
		case what of
			0	:	if wsorsq[0]*wsorsq[1] lt wavlsq then $
					message, 'Unphysical X parameters!'
			1	:	wsorsq[1] = wsorsq[1] + wavlsq/wsorsq[0]
			2	:	wsorsq = wsorsq + beam.wavl*[clen,1/clen]
		endcase
		beam.xsec[0].bpars[0] = 1.
		beam.xsec[0].bpars[1:2] = wsorsq
		beam.xsec[0].elset = 1
	endif

	if (whi and 2) ne 0 then begin
		beam.ysec[0].optel = 'psr'
		wsorsq = smult*(sizfac*ysor)^2
		case what of
			0	:	if wsorsq[0]*wsorsq[1] lt wavlsq then $
					message, 'Unphysical Y parameters!'
			1	:	wsorsq[1] = wsorsq[1] + wavlsq/wsorsq[0]
			2	:	wsorsq = wsorsq + beam.wavl*[clen,1/clen]
		endcase
		beam.ysec[0].bpars[0] = 1.
		beam.ysec[0].bpars[1:2] = wsorsq
		beam.ysec[0].elset = 1
	endif

	return, FPU_fix(beam)
end