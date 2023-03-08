Function PQR_pre, psq, qsq, r, first = fir, second = sec, length= len, type= typ

;+
; NAME:
;		PQR_PRE
; VERSION:
;		8.61
; PURPOSE:
;		Prepares input data for the PQR routines.
; CATEGORY:
;		Optics calculation.
; CALLING SEQUENCE:
;		Result = PQR_PRE( PSQ [, QSQ, R], FIRST = FIR, SECOND = SEC, TYPE = TYP)
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
;		Note2:	If all three inputs are provided they must all scalars or all
;				vectors of same length.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	FIRST
;		Optional output, see below.
;	SECOND
;		Optional output, see below.
;	LENGTH
;		Optional output, see below.
;	TYPE
;		Optional output, see below.
; OUTPUTS:
; 		Possible return values are:
; 			0	: 	If no proper PQR data is provided at input.
; 			1	:	If one dimensional PQR data is provided.  It can be in the
; 					form of a 3-element vector, [3,N] array or 3 separate
; 					vectors of same length.
; 			2	:	If 2 dimensional PQR data, in the form of a 6-element vector
; 					or a [6,N] array, is provided.
; OPTIONAL OUTPUT PARAMETERS:
;	FIRST
;		If the return value is 0 (i.e. no valid data), returns !NULL, else
;		returns PQR data packed into a [3,N] array (if the data was provided in
;		this form to begin with, it is returned as is).
;	SECOND
;		If the return value is 0 or 1, returns !NULL, else returns the second
;		set of PQR data.
;	LENGTH
;		Returns the length of the output data set(s) meaning, if FIRST and,
;		possibly, SECOND, are [3,N] arrays, the return value is N.  If they're
;		3 dimensional vectors (equivalent to [3,1], returns 1.
;	TYPE
;		Returns the larger of 4 and the Type of the input data.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1:	The restrictions on the input data, as listed above.
;		2:	P and Q values must be non-negative.  This encompasses the PSQ and
;		QSQ vectors, if provided separately, the first two columns of PSQ when
;		provided as a [3,N] array or the first 4 columns of PSQ when provided as
;		a [6,N] array.
; PROCEDURE:
;		Straightforward.  Calls CALCTYPE, CAST and CODIMS, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-JAN-2018 by Mati Meron.
;-

	on_error, 1

	typ = Calctype(0.,psq,qsq,r,def=4)
	fir = (sec = [])
	res = 0

	if Codims(psq,qsq,r,/same,ninp=nin,dims=dim) then begin
		case nin of
			1	:	begin
						if n_elements(dim) le 2 then begin
							wpsq = Cast(reform(psq),typ)
							case dim[0] of
								3	:	begin
											if min(wpsq[0:1,*]) ge 0 then begin
												fir = wpsq
												res = 1
											endif else message, $
											'Invalid input!', /con
										end
								6	:	begin
											if min(wpsq[0:3,*]) ge 0 then begin
												fir = wpsq[[0,2,4],*]
												sec = wpsq[[1,3,5],*]
												res = 2
											endif else message, $
											'Invalid input!', /con
										end
								else:	message, 'Invalid input dimensions!', $
									 	/con
							endcase
						endif else message,'no more than 2 dims. allowed!', /con
					end
			3	:	begin
						if n_elements(dim) eq 1 then begin
							if min(psq) ge 0 and min(qsq) ge 0 then begin
								fir = Cast(transpose($
								[[reform(psq)],[reform(qsq)],[reform(r)]]),typ)
								res = 1
							endif else message, 'Invalid input!', /con
						endif else message, 'Inconsistent input dimensions!', $
						/con
					end
			else:	message, 'Bad input!', /con
		endcase
	endif

	if res gt 0 then begin
		rdim = size(fir,/dim)
		if n_elements(rdim) eq 2 then len = dim[1] else len = 1
	endif else len = 0
		
	return, res
end