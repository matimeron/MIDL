Function Focspot, bsec, targ, focmin = fcm, internal = int

;+
; NAME:
;		FOCSPOT
; VERSION:
;		5.0
; PURPOSE:
;		Calculates minimal focal spot, for a given beam and distance.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = FOCSPOT( BSEC, TARG, [FOCMIN = FCM] [, INTERNAL = INT])
; INPUTS:
;	BSEC
;		Standard beam cross-section (see BEAMSEC_DEFINE).  A 4-element float
;		vector, includes in order, the following:
;			0	:	relative intensity.
;			1	:	square of source linear size.
;			2	:	square of source angular size.
;			3	:	current distance from source.  Note that this distance is
;					from the "optical source", not the physical one.
;	TARGET
;		Scalar or vector, the distance (in meters) to the focal point.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FOCMIN
;		Numeric scalar, sets a minimal value for the focal length.  If the
;		focal length for a given value of TARGET evaluates to less then FOCMIN,
;		it is replaced by FOCMIN in the calculation (note,in this case the spot
;		size obtained is larger then the theoretical minimum.  The default value
;		of FOCMIN is zero.
;	/INTERNAL
;		Switch.  Selects, on input, between internal (meter, radian) and
;		external (micron, microradian) units.  Output is always in external
;		units.
; OUTPUTS:
;		Returns the resulting spot size(s) in same format as TARGET.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		Block BEAM_STUFF, as defined in INIT_OPT.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Straightforward.  Follows Sec. 3 from SOR.  Calls HSIZE, FOCLN, FOCUS
;		and FPROP from OPT_LIB.  Also calls CAST and DEFAULT from MIDL.
; MODIFICATION HISTORY:
;		Created 10-AUG-2005 by Mati Meron.
;-

	on_error, 1
	common beam_stuff, exs, npomax, mmicr, mwlen

	mult = replicate(1.,4)
	if not keyword_set(int) then mult[1:2] = mmicr^2
	wsec = mult*bsec

	nr = n_elements(targ)
	res = Cast(targ,4)
	for i = 0l, nr - 1 do begin
		fcl = Focln(wsec,targ[i]) > Default(fcm,0.,/dtyp)
		res[i] = Hsize(Fprop(0,Focus(0,wsec,fcl),targ[i]))
	endfor

	return, res
end