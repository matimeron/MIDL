Function Focln, bsec, targ, focus_type = fot

;+
; NAME:
;		FOCLN
; VERSION:
;		4.2
; PURPOSE:
;		Calculates required focal length, for a given beam and distance.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = FOCLN( BSEC, TARG, [FOCUS_TYPE = FOT])
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
;		Scalar value, the distance (in meters) to the focal point.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FOCUS_TYPE
;		Accepts either a numeric (integer) or a character value.  Possible
;		values and corresponding character codes (only first 2 letters matter
;		are:
;			1 ('hsiz')	:	Minimizes beam size at TARGET.  Not a true focus.
;			2 ('colm')	:	Collimation, i.e. focusing to infinity.  TARGET
;							is ignored in this case.
;			3 ('ssiz')	:	Focus with small physical size at TARGET.
;			4 ('asiz')	:	Focus with small angular size at TARGET.
;		Note:	For a given TARGET distance there are two different focal
;				lengths which give a focus (virtual source) at target.  One
;				of them yields smaller spatial size, the other smaller angular
;				size.  These two lengths are given by (3) and (4), respectively
; OUTPUTS:
;		Returns the focal length required to achieve focusing at TARGET.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		For 3 ('ssiz') and 4 ('asiz') no focusing is possible for distances
;		exceeding (beam_size)^2/(2*emittance).
; PROCEDURE:
;		Straightforward.  Follows Sec. 3 from SOR.  Calls EMITT and HSIZE from
;		OPT_LIB.  Also calls ISNUM, STRMATCH_MM and TYPE from MIDL.
; MODIFICATION HISTORY:
;		Created 20-AUG-2001 by Mati Meron.
;		Modified 25-MAY-2002 by Mati Meron.
;-

	on_error, 1

	posib = ['hsiz','colm','ssiz','asiz']
	if Type(fot) eq 0 then ftyp = 1l $
	else if Isnum(fot) then ftyp = long(fot) $
	else ftyp = 1 + Strmatch_mm(fot,posib,2)
	hsq = Hsize(bsec,/int)^2

	case (ftyp + 1)/2 of
		1	:	begin
					if ftyp eq 1 then $
					res = targ*hsq/(targ*bsec[2]*bsec[3] + hsq) $
					else res = hsq/(bsec[2]*bsec[3])
				end
		2	:	begin
					emt = Emitt(bsec,/int)
					tarmax = hsq/(2*emt)
					if targ gt tarmax then message, "Can't focus beyond " + $
					String(tarmax, form = "(f8.3,' m.')")
					fterm = hsq + 2*targ*bsec[2]*bsec[3]
					sterm = sqrt(hsq^2 - (2*targ*emt)^2)
					if ftyp eq 3 then res = 2*targ*hsq/(fterm + sterm) $
					else res = (fterm + sterm)/(2*bsec[2]*(targ + bsec[3]))
				end
		else:	message, 'Unknown focusing type!'
	endcase

	return, res
end