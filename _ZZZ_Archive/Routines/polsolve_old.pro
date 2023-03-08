Function Polsolve_old, coef

;+
; NAME:
;		POLSOLVE
; VERSION:
;		8.15
; PURPOSE:
; 		Finds the roots of a polynomial equation of the form
; 			c(0) + c(1)*X + ... + c(n)*X^n, with 1 <= n <= 4. 
; CATEGORY:
;		Mathemetical function (general).
; CALLING SEQUENCE:
;		Result = POLSOLVE( COEF)
; INPUTS:
;	COEF
;		Numeric vector, length 2-5, representing the coefficients of a 
;		polynomial.  Mandatory.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;		None.
; OUTPUTS:
;		Returns a numeric vector containing the roots of the polynomial 
;		equation.  The output type is determined as follows:
;		
;		For COEF type FLOAT or below, the output is FLOAT if all the roots are
;		real, else it is COMPLEX.
;		
;		For COEF type DOUBLE the output is DOUBLE if all the roots are
;		real, else it is DCOMPLEX.
;
;		For COEF type COMPLEX or DCOMPLEX, the output is of same type.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		The equation order must be 1-4, thus the length of COEF must be 2-5.
; PROCEDURE:
;		Exact algebraic calculation.  Calls ABS_MM, ARREQ, CALCTYPE, CAST, 
;		IMAGINARY_MM, ISNUM, LEXISORT, REAL_MM, SIGN and TOLER, from MIDL.  
;		Calls itself recursively when needed.
; MODIFICATION HISTORY:
;		Created 15-APR-2012 by Mati Meron.
;-

	on_error, 1

	ord = n_elements(coef) - 1
	if ord gt 0 then begin
		eps = Toler(coef)
		typ = Calctype(coef,0.)
		ocfl = (icfl = Isnum(coef,/complex))
		wtyp = 9
		c = Cast(coef,wtyp)
		while ord gt 0 and c[ord] eq 0 do begin
			ord = ord - 1
			c = c[0:ord]
		endwhile
	endif else message, 'Missing or insufficient input!'

	case ord of
		0	:	message, 'Zero order, not acceptable'
		1	:	res = - c[0]/c[1]
		2	:	begin
					disc = c[1]^2 - 4*c[0]*c[2]
					ocfl = icfl or Real_mm(disc) lt 0 
					fac = -(c[1] + Sign(c[1],/noz)*sqrt(disc))/2
					if fac eq 0 then res=[0.,0.] else res=[fac/c[2],c[0]/fac]
				end
		3	:	begin
					fir=(2*c[2]^3- 9*c[1]*c[2]*c[3]+ 27*c[0]*c[3]^2)/(54*c[3]^3)
					sec = (c[2]^2 - 3*c[1]*c[3])/(9*c[3]^2)
					ocfl =icfl or Real_mm(fir^2-sec^3) gt 0 or Real_mm(sec) lt 0
					phas = 2*!dpi*[-1,0,1]
					if Abs_mm(sec) gt eps*Abs_mm(fir) then begin
						psi = (acos(fir/sec^(3./2)) + phas)/3
						res = -(c[2]/(3*c[3]) + 2*sqrt(sec)*cos(psi))
					endif else res = -(c[2]/(3*c[3]) + $
						(2*fir)^(1/3d)*exp(dcomplex(0,phas/3)))
				end
		4	:	begin
					d = (dzer = make_array(3,type=typ))
					d[0] = (256*c[0]*c[4]^3 - 64*c[1]*c[3]*c[4]^2 + $
							16*c[2]*c[3]^2*c[4] - 3*c[3]^4)/(256*c[4]^4)
					d[1] = (8*c[1]*c[4]^2 - 4*c[2]*c[3]*c[4]+ c[3]^3)/(8*c[4]^3)
					d[2] = (8*c[2]*c[4] - 3*c[3]^2)/(8*c[4]^2)
					psq = Polsolve([-d[1]^2,d[2]^2-4*d[0],2*d[2],1])
					if not Arreq(psq,dzer) then begin
						p = sqrt(dcomplex(psq))						
						pin=where(abs(Imaginary_mm(p)) lt eps*abs(Real_mm(p)),n)
						case n of
							0	:	dum = max(Abs_mm(p),pin)
							1	:	pin = pin[0]
							else:	begin
										dum = max(abs_mm(p[pin]),ppin)
										pin = pin[ppin]
									end
						endcase
						p = p[pin]
						if p eq 0 then begin
							dum = max(Abs_mm(p),pin)
							p = p[pin]
						endif
						if abs(Imaginary_mm(p)) lt eps*abs(Real_mm(p)) and $
						not icfl then p = Real_mm(p)
						q = (p^2 + d[2] - d[1]/p)/2
						fres = Polsolve([q,p,1])
						q = (p^2 + d[2] + d[1]/p)/2
						sres = Polsolve([q,-p,1])
						res = [fres,sres]
					endif else res = make_array(4,type=typ)
					ocfl = icfl or Isnum(res,/complex)
					res = res - c[3]/(4*c[4])					
				end
		else:	message, 'No analytical solution beyond 4th order'
	endcase

	re = Real_mm(res)
	im = Imaginary_mm(res)
	eps = max(abs(coef))*eps
	dum = where(abs(re) lt eps, ndum)
	if ndum gt 0 then res[dum] = dcomplex(0,im[dum])
	dum = where(abs(im) lt eps, ndum)
	if ndum gt 0 then res[dum] = dcomplex(re[dum],0)

	if ocfl ne icfl then typ = ((typ + 1)/2)*3
	res = Cast(res,typ,typ,/fix)
	re = Real_mm(res)
	im = Imaginary_mm(res)
	s = Lexisort(re,im)

	return, res[s]
end