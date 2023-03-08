Function Stirling, n, m, k

;+
; NAME:
;		STIRLING
; VERSION:
;		8.425
; PURPOSE:
;		Calculates Stirling numbers, of first or second kind.
;		
;		Note:	The usefulness of Stirling numbers is in the following:
;		
;			1)	x(x-1)...(x-n+1) = Sum{m=0...n}[S_(n,m,1)x^m]
;			2)	x^n = Sum{m=0...n}[S_(n,m,2)x(x-1)...(x-m+1)
;			
;			In case (1) Stirling numbers of the first kind are used, in case (2)
;			Stirling numbers of the second kind.
; CATEGORY:
;		Mathematical.
; CALLING SEQUENCE:
;		Result = STIRLING( N, M, K)
; INPUTS:
;	N
;		Non-negative integer scalar.
;	M
;		Non-negative integer scalar, must be <= N.
;	K
;		Integer scalar, only acceptable values are 1 and 2.  Denotes the "kind"
;		of Stirling numbers required.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
; 		None.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		For Stirling numbers of first kind results are fully accurate up to 
;		N = 12.  For the second kind they remain accurate up to N = 23.
; PROCEDURE:
;		Straightforward from definitions.  Calls itself recursively (when 
;		calculating numbers of the first kind).  Calls BINCOEF, DEFAULT and
;		ISNUM, from MIDL.
; MODIFICATION HISTORY:
;		Created 25-AUG-2015 by Mati Meron.
;-

	on_error, 1
	lim = 2ll^63-1

	if Isnum (n,/int) and Isnum(m,/int) and (n < m) ge 0 and (n ge m) then begin
		case Default(k,0,/dtyp) of
			1	:	begin
						vlfl = (n ge 14)
						p = lindgen(n-m+1)
						fir = (sec = lon64arr(n-m+1))
						ovfl = [0,0]
						for q = 0, n-m do begin
							tem = Bincoef(n-1+q,n-m+q,/int)
							if tem gt lim and not ovfl[0] then begin
								fir = double(fir)
								ovfl[0] = 1
							endif
							fir[q] = tem
							tem = Stirling(n-m+q,q,2)
							if tem gt lim and not ovfl[1] then begin
								sec = double(sec)
								ovfl[1] = 1
							endif
							sec[q] = tem
						endfor
						res = total((-1d)^p*fir*sec*Bincoef(2*n-m,n-m-p,/int))
					end
			2	:	begin
						vlfl = (n ge 16)
						p = lindgen(m+1)
						dp = double(p)
						res = $
						total((-1)^(m-p)*Bincoef(m,p,/int)*dp^n/factorial(m))
					end
			else:	message, '"Kind" value must be present and equal 1 or 2'
		endcase
	endif else message, 'Bad or missing input(s)!'
	if abs(res) le lim then res = round(res,l64=vlfl)

	return, res
end