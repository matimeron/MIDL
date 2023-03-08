Function Gram_Schmidt, u_0, u_1, u_2, u_3, u_4, u_5, u_6, u_7, $
						u_8, u_9, u_10, u_11, u_12, u_13, u_14, u_15, $
	v00=v_0, v01=v_1, v02=v_2, v03=v_3, v04=v_4, v05=v_5, v06=v_6, v07=v_7, $
	v08=v_8, v09=v_9, v10=v_10, v11=v_11, v12=v_12, v13=v_13, v14=v_14, v15=v_15
	
;+
; NAME:
;		GRAM_SCHMIDT
; VERSION:
;		8.41
; PURPOSE:
;		Performing a Gram_Schmidt orthonormalization of a set of vectors.
; CATEGORY:
;		Mathematical, array processing.
; CALLING SEQUENCE:
;		Result = GRAM_SCHMIDT( U_0, .... [, V00 = V_0, ...])
; INPUTS:
;	U_0, ..
;		Numeric vectors or arrays of arbitrary (but same) dimensionality.
;		Maximal number accepted is currently 16 but can be extended.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	V00, ..
;		Optional outputs, see below.
; OUTPUTS:
;		Returns an [N,N] matrix (where N is the number of the inputs) which,
;		given an arbitrary vector's representation in terms of the original, 
;		input base, transforms it into a representation in terms of the 
;		orthogonal output base.  In other words, given a vector W which can be
;		repesented as
;			W = a0*U_0 + a1*U_1 + ...a(n-1)*U_(n-1)
;		or
;			W = b0*V_0 + b1*V_1 + ...b(n-1)*V_(n-1)
;		
;		then the vector [b0,..b(n-1)] is obtained from [a0,..a(n-1)] through
;		multiplication by the result matrix.
; OPTIONAL OUTPUT PARAMETERS:
;	V00, ..
;		Returning the base vectors of the orthogonalized base.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		1)	All the inputs have to be of same dimensionality.
;		2)	The (constant) number of elements in each input must be >= the
;			number of inputs.
; PROCEDURE:
;		Straightforward.  Calls ARREQ, CAST (indirectly), TYPE (ditto) and 
;		NPARDEF, from MIDL.
; MODIFICATION HISTORY:
;		Created 10-APR-2015 by Mati Meron.
;-

	on_error, 1

	nmx = 16
	unam =  strcompress('u_' + sindgen(nmx),/remove)
	vnam =  strcompress('v_' + sindgen(nmx),/remove)
	si = string(indgen(nmx),form='(i0)')

	n = Npardef(u_0, u_1,  u_2,  u_3,  u_4,  u_5,  u_6,  u_7, $
		u_8, u_9, u_10, u_11, u_12, u_13, u_14, u_15, which = whi)
	if n eq 0 then message, 'Inputs?!'
	if not Arreq(whi,lindgen(n)) then message, 'Undefined inputs!'
	if n gt n_elements(u_0) then message, 'Too many inputs, not independent!'

	typ = 4
	fdim = size(u_0,/dim)
	for i = 0, n-1 do begin
		dum = execute('typ = typ > Type(' + unam[i] + ')')
		dum = execute('dim = size(' + unam[i] + ',/dim)')
		if not Arreq(fdim,dim) then message, 'Inconstant dimensions!
		dum = execute(vnam[i] + ' = double(' + unam[i] +')')
	endfor

	for i = 0, n-1 do begin
		dum = execute(vnam[i]+ '='+ vnam[i]+ '/sqrt(total('+ vnam[i]+ '^2))')
		for j = i+1, n-1 do dum = execute(vnam[j]+ '='+ vnam[j] + $
				'-total('+ vnam[j]+ '*'+ vnam[i]+ ')*'+ vnam[i])
	endfor

	res = make_array(n,n,typ=typ)
	styp = string(typ,form='(i0)')
	for j = 0, n-1 do begin
		for i = 0, j do begin
			dum = execute('prod = total(' + vnam[i] + '*' + unam[j] + ')')
			res[i,j] = prod
		endfor
		dum = execute(vnam[j]+ '= Cast('+ vnam[j]+ ','+ styp+ ','+ styp+ ')')
	endfor

	return, res
end