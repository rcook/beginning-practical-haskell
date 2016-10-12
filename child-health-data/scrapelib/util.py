def unpack_args(*args):
  l = list(args)
  return l[0] if len(l) == 1 and isinstance(l[0], list) else l
