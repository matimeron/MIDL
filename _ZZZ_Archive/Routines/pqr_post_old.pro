Function PQR_post_old, fir, sec, type = typ, rpsq = rpsq, rqsq = rqsq, rr = rr

;+
; NAME:
;		PQR_POST
; VERSION:
;		8.61
; PURPOSE:
;		Combines PQR data.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = PQR_POST( FIR [, SEC] [, keywords])
; INPUTS:
;	FIR
;		1D PQR data, i.e. a 3 element vector or a [3,N] array.
; OPTIONAL INPUT PARAMETERS:
;	SEC
;		Same as FIR.  If given, must be of same length as FIR.
; KEYWORD PARAMETERS:
;	TYPE
;		Integer scalar representing data type.  If given, the result is cast to
;		this type, else its type will be the higher of the types of FIR and SEC
;		(if present) but no lower than 4 (FLOAT).
;	RPSQ
;		Optional output, see below.
;	RQSQ
;		Optional output, see below.
;	RR
;		Optional output, see below.
; OUTPUTS:
; 		If both FIR and SEC are given, the output is 2D PQR data, i.e. a [6,*]
; 		array, where the first two columns are P^2 values, the next two are Q^2
; 		values and the last two are R values.  Else, if only FIR is given, it is
; 		returned as the result (possibly cast to the appropriate type).
; OPTIONAL OUTPUT PARAMETERS:
;	RPSQ
;		Returns the value(s) of the P^2 parameter.
;	RQSQ
;		Returns the value(s) of the Q^2 parameter.
;	RR
;		Returns the value(s) of the R parameter.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1:	The restrictions on the input data, as listed above.
; PROCEDURE:
;		Straightforward.  Calls ARREQ, CALCTYPE, CAST and ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-JAN-2018 by Mati Meron.
;-

	on_error, 1

	if not Isnum(typ) then typ = Calctype(0.,fir,sec,def=4)
	pfl = arg_present(rpsq)
	qfl = arg_present(rqsq)
	rfl = arg_present(rr)

	case n_params() of
		0	:	message, 'Missing input(s)!'
		1	:	begin
					fdim = size(fir,/dim)
					if n_elements(fdim) le 2 and fdim[0] eq 3 then begin
						res = Cast(fir,typ,typ)
						if pfl then rpsq = res[0,*] else rpsq = !null
						if qfl then rqsq = res[1,*] else rqsq = !null
						if rfl then rr = res[2,*] else rr = !null
					endif else message, 'Invalid input dimensions!'
				end
		2	:	begin
					fdim = size(fir,/dim)
					if n_elements(fdim) le 2 and fdim[0] eq 3 then begin
						sdim = size(sec,/dim)
						if Arreq(fdim,sdim) then begin
							res = make_array([fdim]*[2,1],typ=typ)
							res[[0,2,4],*] = fir
							res[[1,3,5],*] = sec
							if pfl then rpsq = res[[0,1],*] else rpsq = !null
							if qfl then rqsq = res[[2,3],*] else rqsq = !null
							if rfl then rr = res[[4,5],*] else rr = !null
						endif
					endif
				end
	endcase

	return, res
end