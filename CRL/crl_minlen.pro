Function CRL_minlen, lvals, qvals, qerrs, minq = minq

	on_error, 1

	if Codims(lvals,qvals,qerrs,/same,dim=dim) then begin
		if Isnum(qerrs) then ferrs = 2*qvals*qerrs else ferrs = []
		res = Parext(lvals,qvals^2,ferrs,ext=qsq,error=err)
		minq = sqrt(qsq)
	endif else message, 'Inconsistent inputs!'

	return, res
end
	 