Function Factors, n, pack = pac

;+
; NAME:
;		FACTORS
; VERSION:
;		8.714
; PURPOSE:
;		Finds the factors of an integer input.
; CATEGORY:
;		Mathematical function (general).
; CALLING SEQUENCE:
;		List = FACTORS( N [, /PAC)
; INPUTS:
;	N
;		Integer scalar.  If negative, converted to positive on input.
; OPTIONAL INPUT PARAMETERS:
;		None.
; KEYWORD PARAMETERS:
;	/Pack
;		Switch.  Changes output format, see below in outputs.
; OUTPUTS:
;		Returns the list of prime factors of N, in ascending order.  Possible
;		output formats are:
;			Default	:	The output is a vector, with each prime factor appearing
;						as many times as it appears in the decomposition of N.
;			PACK set:	The output is a 2-column array, where in the first
;						column each prime factor appears once, and in the second
;						column appears its multiplicity.
;		For example The default output of FACTORS(200) is the array [2,2,2,5,5].
;		With PACK set, the output will be the 2D array:
;			2	3
;			5	2
;
;		The data type of the result is UNSIGNED LONG, except when N is of type
;		64-BIT LONG, or 64-BIT UNSIGNED LONG, in which case the result will be
;		64-BIT UNSIGNED LONG.
;
;		If N = 0 or 1, the return value is !NULL.
; OPTIONAL OUTPUT PARAMETERS:
;		None.
; COMMON BLOCKS:
;		None.
; SIDE EFFECTS:
;		None.
; RESTRICTIONS:
;		Inputs must be of integer type.
; PROCEDURE:
;		Straightforward check for dividibility, using primes table.
;		Calls ISNUM, PRINUMS, SORPURGE and TYPE, from MIDL.
; MODIFICATION HISTORY:
;		Created 20-MAY-2019 by Mati Meron.
;-

	on_error, 1

	if Isnum(n,/int) then begin
		bfl = Type(n) gt 13
		if bfl then abn = ulong64(abs(n)) else abn = ulong(abs(n))
		res = []
		if abn gt 3 then begin
			lis = Prinums(floor(sqrt(abn)))
			if bfl then lis = ulong64(lis) else lis = ulong(lis)
			done = 0
			repeat begin
				tabn = abn/lis[0]
				if abn - tabn*lis[0] eq 0 then begin
					abn = tabn
					res = [res,lis[0]]
					dum = where(lis le abn,ndum)
					if ndum gt 0 then lis = lis[dum] else done = 1
				endif else begin
					if n_elements(lis) eq 1 then begin
						res = [res,abn]
						done = 1
					endif else lis = lis[1:*]
				endelse
			endrep until done
		endif else if abn gt 1 then res = abn
		if keyword_set(pac) and Type(res) ne 0 then begin
			fres = res
			sres = res[Sorpurge(res,net=net)]
			res = make_array(2,net,typ=Type(res))
			for i = 0, net-1 do begin
				dum = where(fres eq sres[i],ndum)
				res[*,i] = [sres[i],ndum]
			endfor
		endif
	endif else message, 'Integer type input required!'

	return, res
end