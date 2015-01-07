uniform sampler2D texture;

const float threshold = 0.15;
const float multiplier = 5.0;

void main()
{
  vec4 sourceFragment = texture2D(texture, gl_TexCoord[0].xy);
  float luminance = sourceFragment.r * 0.5 + sourceFragment.g * 0.6 + sourceFragment.b * 0.5;
  sourceFragment *= clamp(luminance - threshold, 0.0, 1.0) * multiplier;
  gl_FragColor = sourceFragment;
}
