Pro Ksee, kmax, l, _extra = _e

	on_error, 1

	if l ge 3 and l mod 2 eq 1 then begin
		lp = l
		lm = l-2
		kp = Make_grid([0,kmax],500)
		km = sqrt((lm*kp^2 - 4)/lp > 0)
		fp = 1/(1 + kp^2/2)
		fm = 1/(1 + km^2/2)
		wp = kp*fp
		wm = km*fm
		up = kp*wp/4
		um = km*wm/4
		bp = FPU_fix((lp*wp*Jsd_fun(up,lp,/neg))^2)
		bm = FPU_fix((lm*wm*Jsd_fun(um,lm,/neg))^2)
		resp = transpose([[kp],[bp]])
		resm = transpose([[kp],[bm]])
		Scan_show, resp, resm, thi=2, _extra = _e
	endif else message, 'L must be odd and >=3'

	return
end