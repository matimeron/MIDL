Function Opvar_conv, xaz = xaz, pqr = pqr, unsquared = uns

;+
; NAME:
;		OPVAR_CONV
; VERSION:
;		8.72
; PURPOSE:
;		Converts between two sets of optical parameters
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = OPVAR_CONV ( XAZ = XAZ {or} PQR = PQR)
; INPUTS:
;		None.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	XAZ
;		Numeric array, a set of optical parameters in the		|
;		XAZ(sq), i.e. [X-size^2, Ang-size^2, Z-coord] format	|	One and
;		Can be	either a 3-element vector or a [3,N] array,		|	only one of
;		where each triplet serves as individual XAZ(sq) set.	|	these two
;	PQR															|	must be
;		Numeric array, a set of optical parameters in the		|	provided.
;		PQR(sq), i.e. [emittance^2, beam size^2, Z-scaled ang.	|
;		size) format.  Possible data formats same as for XAZ	|
;	/UNSQUARED
;		Switch.  If set, a square root is taken of the first two columns of the
;		result which, therefore, become [X-size, Y-size,..] or
;		[emittance, beam size, ...], according to the option used.
; OUTPUTS:
;		if XAZ data is provided, returns this data converted to the PQR format,
;		and vice versa.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None other than the input format requirements (3-elem vector or a [3,N]
;		array).
; PROCEDURE:
;		Straightforward from definitions (elsewhere).  Calls CAST and ONE_OF
;		from MIDL.
; MODIFICATION HISTORY:
;		Created 15-JUN-2016 by Mati Meron.
;		Modified 25-APR-2017 by Mati Meron.  Comment block changes.
;		Modified 10-AUG-2020 by Mati Meron.  Added keyword UNSQUARED.
;-

	on_error, 1

	wha = One_of(xaz,pqr,val=in)
	if wha ge 0 then win = Cast(in,4) else message, 'Missing input!'
	siz = size(win,/dim)
	if siz[0] eq 3 then begin
		case n_elements(siz) of
			1	:	begin
						case wha of
							0	:	res = [win[0]*win[1], $
										win[0] + win[1]*win[2]^2, $
										win[1]*win[2]]
							1	:	begin
										tem = win[0] + win[2]^2
										res = [win[0]*win[1]/tem, $
											tem/win[1], $
											win[1]*win[2]/tem]
									end
						endcase
					end
			2	:	begin
						case wha of
							0	:	res = [win[0,*]*win[1,*], $
										win[0,*] + win[1,*]*win[2,*]^2, $
										win[1,*]*win[2,*]]
							1	:	begin
										tem = win[0,*] + win[2,*]^2
										res = [win[0,*]*win[1,*]/tem, $
											tem/win[1,*], $
											win[1,*]*win[2,*]/tem]
									end
						endcase
						res = reform(res)
					end
			else:	message, 'Bad input!'
		endcase	
	endif else message, 'First dimension of input array must be 3!'
	if keyword_set(uns) then res[0:1,*] = sqrt(res[0:1,*])

	return, res
end