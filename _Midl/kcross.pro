Function Kcross, k, l

	on_error, 1

	if l ge 3 and l mod 2 eq 1 then begin
		lp = l
		lm = l-2
		kp = Cast(k,4)
		km = sqrt((lm*kp^2 - 4)/lp > 0)
		wp = kp/(1 + kp^2/2)
		wm = km/(1 + km^2/2)
		up = kp*wp/4
		um = km*wm/4
		bp = (lp*wp*Jsd_fun(up,lp,/neg))^2
		bm = (lm*wm*Jsd_fun(um,lm,/neg))^2
		res = bp - bm
	endif else message, 'L must be odd and >=3'

	return, FPU_fix(res)
end