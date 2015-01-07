uniform sampler2D texture;
uniform vec2 dir;
uniform vec2 pix_size;

void main()
{
  vec2 tc  = gl_TexCoord[0].xy;
  vec2 ps  = pix_size;
  vec4 color = vec4(0.0);
  color += texture2D(texture, vec2(tc.x - 4.0 * dir.x * ps.x, tc.y - 4.0 * dir.y * ps.y)) * 0.0162162162;
  color += texture2D(texture, vec2(tc.x - 3.0 * dir.x * ps.x, tc.y - 3.0 * dir.y * ps.y)) * 0.0540540541;
  color += texture2D(texture, vec2(tc.x - 2.0 * dir.x * ps.x, tc.y - 2.0 * dir.y * ps.y)) * 0.1216216216;
  color += texture2D(texture, vec2(tc.x - 1.0 * dir.x * ps.x, tc.y - 1.0 * dir.y * ps.y)) * 0.1945945946;
  color += texture2D(texture, tc) * 0.2270270270;
  color += texture2D(texture, vec2(tc.x + 1.0 * dir.x * ps.x, tc.y + 1.0 * dir.y * ps.y)) * 0.1945945946;
  color += texture2D(texture, vec2(tc.x + 2.0 * dir.x * ps.x, tc.y + 2.0 * dir.y * ps.y)) * 0.1216216216;
  color += texture2D(texture, vec2(tc.x + 3.0 * dir.x * ps.x, tc.y + 3.0 * dir.y * ps.y)) * 0.0540540541;
  color += texture2D(texture, vec2(tc.x + 4.0 * dir.x * ps.x, tc.y + 4.0 * dir.y * ps.y)) * 0.0162162162;
  gl_FragColor = color; 
}


