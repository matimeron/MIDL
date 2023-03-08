Function Binrep, num

    idig = 32b
    ns = size(num)
    ntot = ns(ns(0) + 2)
    inum = lonarr(ntot) + long(num)
    firdig = replicate(idig - 1,ntot)
    digits = bytarr(ntot,idig)

    i = idig
    dum = where(inum gt 0, idum)
    while idum ne 0 do begin
	i = i - 1
	digits(*,i) = inum mod 2
	firdig(dum) = i
	inum = inum/2
	dum = where(inum gt 0, idum)
    endwhile

    digits = transpose(digits)
    res = string(digits + 48b)
    dmin = min(firdig, max = dmax)
    for i = dmin, dmax do begin
	dum = where(firdig eq i, idum)
	if idum ne 0 then res(dum) = strmid(res(dum),i,idig + 1 - i)
    endfor

    if ns(0) eq 0 then return, res else return, reform(res,ns(1:ns(0)))
end
