Function Harcheck, gtet, k, har = l

	on_error, 1

	tem = gtet^2 + 0.5*k^2 + 1
	u = 0.25*k^2/tem
	v = 2.*k*gtet/tem

	res = 0*gtet
	for i = 0, n_elements(gtet)-1 do res[i] =Ug_harm(l,u(i),v(i),k,gtet(i),/ver)

	return, res
end