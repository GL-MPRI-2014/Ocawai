uniform sampler2D texture;
uniform sampler2D blurred;

vec4 AdjustSaturation(in vec4 color, in float saturation)
{
	float grey = dot(color, vec4(vec3(0.3, 0.59, 0.11), 0.0));
	vec4 grey_color = vec4(grey, grey, grey, 0.0);
	
	return mix(grey_color, color, saturation);
}

void main() 
{
  vec2 tc = gl_TexCoord[0].xy;
  vec4 bloom = AdjustSaturation(texture2D(blurred, tc), 1.5);
  vec4 base  = AdjustSaturation(texture2D(texture, tc), 0.3);
  gl_FragColor = base * 2. + bloom * 2.;
}
