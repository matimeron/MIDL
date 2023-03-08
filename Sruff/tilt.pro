Function Tilt, ang, degrees = deg, xvec = x, yvec = y, zvec = z, $
    xygrid = xy, area = arel, power = pow, xtilt = xti, tiltstring = tilst

    on_error, 1

    if n_elements(ang) eq 0 then sang = 1. else $
    if keyword_set(deg) then sang = sin(!dtor*ang) else sang = sin(ang)

    if keyword_set(xti) then ti = 0 else ti = 1
    strs = ['X tilt, at','Y tilt, at']

    if sang ne 1 then begin
	if n_elements(x) ne 0 and not ti then x = 1./sang*x
	if n_elements(y) ne 0 and ti then y = 1./sang*y
	if n_elements(z) ne 0 then z = sang*z
	if n_elements(xy) ne 0 then xy(ti,*,*) = 1./sang*xy(ti,*,*)
	if n_elements(arel) ne 0 then arel = 1./sang*arel
	if n_elements(pow) ne 0 then pow = sang*pow
	tilst = strs(ti) + $
	strcompress(string(!radeg*asin(sang),form='(f7.3)')) + string(154b)
    endif

    return, sang
end
