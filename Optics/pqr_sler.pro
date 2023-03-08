Function PQR_sler, psq, qsq, r, sler = sle, rpsq = rpsq, rqsq = rqsq, rr = rr

;+
; NAME:
;		PQR_SLER
; VERSION:
;		8.6
; PURPOSE:
;		Evaluates slope error broadening in PQR representation.
;		Note: 	This routine is no longer needed, as its function has been
;				included within PQR_PROP.  It is only maintained for legacy
;				purposes.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		result = PQR_SLER, PSQ [, QSQ, R], SLER = SLE, [keywords].
; INPUTS:
;	PSQ
;		Value (or values) of P^2 (or of all the parameters).
;	QSQ
;		Value (or values) of Q^2.
;	R
;		Value (or values) of R.
;
;		Note 1:	If a single input is provided, it is taken to be a "packed"
;				representation of all three parameters.  In this case the input
;				must be one of
;					a)	A 3-element vector or a [3,N] array.
;					b)	A 6-element vector or a [6,N] array.  In this case the
;						input is assumed "doubled", with the first 2-columns
;						being PSQ values, the second two columns QSQ values and
;						the last 2 columns R values.  This option exists for
;						dealing with both transverse beam directions at once.
;		Note2:	If all three inputs are provided they must all scalars, all
;				vectors of same lenght, or a mix of scalars and vectors of same
;				length.  Any other combination is not acceptable.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	SLER
;		Slope error.  Must be in same format (sigma or full width) as the
;		input data.  Default value is 0.
;		
;		Note:	If the input is a [3,N] array, SLER (if defined) must be a 
;				scalar.
;				If the input is a [6,N] array, SLER (if defined) may be a
;				scalar, in which case it is applied only to the second 
;				dimension, or a 2-element vector, in which case the first value
;				is applied to the first dimension and the second to the second.
;	RPSQ
;		Optional output, see below.
;	RQSQ
;		Optional output, see below.
;	RR
;		Optional output, see below.
; OUTPUTS:
;		Returns the values of the P^2, Q^2 and R parameters, following slope
;		error broadening, in a "packed" form, i.e. either a 3-element vector or
;		a [3,N] array.  However, for [6,N] input the result is also in a [6,N]
;		format.
; OPTIONAL OUTPUT PARAMETERS:
;	RPSQ
;		Returns the value(s) of the propagated P^2 parameter.
;	RQSQ
;		Returns the value(s) of the propagated Q^2 parameter.
;	RR
;		Returns the value(s) of the propagated R parameter.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		None.
; PROCEDURE:
;		Follows the derivations in the PQR writeup.  Details elsewhere.  Calls
;		CALCTYPE, CAST, DEFAULT and CODIMS, from MIDL.  May call itself
;		recursively
; MODIFICATION HISTORY:
;		Created 5-AUG-2016 by Mati Meron as a straightforward variation of
;		PQR_PROP.
;		Rendered obsolete on 25-DEC-2017, with its function beeing transfered
;		to PQR_PROP.
;-

	on_error, 1

	typ = Calctype(0.,psq,qsq,r,def=4)
	eps = (machar(doub=(typ gt 4))).epsneg

	skipfl = 0
	case n_params() of
		1	:	begin
					siz = size(psq,/dim)
					if siz[0] eq 3 then begin
						tem = Cast(psq,5)
						case n_elements(siz) of
							1	:	begin
										wpsq = tem[0]
										wqsq = tem[1]
										wr = tem[2]
										dim = 1
									end
							2	:	begin
										wpsq = reform(tem[0,*])
										wqsq = reform(tem[1,*])
										wr = reform(tem[2,*])
										dim = siz[1]
									end
							else:	message, 'Bad input!'
						endcase
					endif else begin
						if siz[0] eq 6 then begin
							case n_elements(sle) of
								0	:
								1	:	begin
											fsle = 0
											ssle = sle[0]
										end
								2	:	begin
											fsle = sle[0]
											ssle = sle[1]
										end
								else:	message, 'Bad slope error input!'
							endcase
							fres = PQR_sler(psq[[0,2,4],*],sle=fsle)
							sres = PQR_sler(psq[[1,3,5],*],sle=ssle)
							rsiz = size(fres)
							rsiz[1] = 6
							res = make_array(size=rsiz)
							res[[0,2,4],*] = fres
							res[[1,3,5],*] = sres
							if arg_present(rpsq) then rpsq = res[0:1,*] $
							else rpsq = !null
							if arg_present(rqsq) then rqsq = res[2:3,*] $
							else rqsq = !null
							if arg_present(rr) then rr = res[4:5,*] $
							else rr = !null
							skipfl = 1
						endif else message, 'Bad input!'
					endelse
				end
		3	:	begin
					if Codims(psq,qsq,r,dim=dim) then begin
						wpsq = Cast(psq,5)
						wqsq = Cast(qsq,5)
						wr = Cast(r,5)
					endif else message, 'Inconsistent input dimensions!'
				end
		else:	message, 'Bad input!'
	endcase
	if skipfl then return, res

	if dim gt 1 then begin
		if n_elements(wpsq) eq 1 then wpsq = replicate(wpsq,dim)
		if n_elements(wqsq) eq 1 then wqsq = replicate(wqsq,dim)
		if n_elements(wr) eq 1 then wr = replicate(wr,dim)
	endif

	if n_elements(sle) gt 1 then message, 'Slope error must be a scalar!'
	tau = (2*Default(sle,0d,/dtyp))[0]
	rpsq = wpsq + tau^2*wqsq
	rqsq = wqsq
	rr = wr

	rpsq = Cast(rpsq,typ,typ)
	rqsq = Cast(rqsq,typ,typ)
	rr = Cast(rr,typ,typ)

	res = transpose([[rpsq],[rqsq],[rr]])

	return, res
end