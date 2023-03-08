Function Hex_dist, rmax, sigma = sig, subtract_mean = sbm

	on_error, 1

	r = []
	wrm = Cast(rmax,5)
	kmax = floor(sqrt((4*wrm^2-1)/3))
	for k = 1l, kmax do begin
		l = dindgen(k + (k+1)/2) - (k-1)/2
		r = [r,k^2+k*l+l^2]
	endfor
	r = sqrt(r[where(r le wrm^2)])
	s = Sorpurge(r,net=rlen)
	res = dblarr(2,rlen+1)
	res[0,1:*] = r[s]
	for i = 1l, rlen do begin
		tem = where(r eq res[0,i],ntem)
		res[1,i] = ntem
	endfor
	res[1,*] = 6*res[1,*]
	res[1,0] = 0

	if keyword_set(sig) then begin
		ores = res
		dr = (wrm - sqrt(wrm^2-1))/4
		r = make_grid([0,wrm],dr,/step,dim=dlen)
		res = dblarr(2,dlen)
		res[0,*] = r
		for i = 1l, rlen do res[1,*] = res[1,*] + $
			ores[1,i]*exp(-((r-ores[0,i])/sig)^2/2)
		res[1,*] = res[1,*]/((2*!dpi)^(3./2)*sig*(r+dr/2))
		if keyword_set(sbm) then res[1,*] = res[1,*] - 2/sqrt(3d)
	endif

	return, FPU_fix(res)
end