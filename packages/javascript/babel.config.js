module.exports = {
  presets: [
    [
      '@babel/preset-env',
      {
        targets: {
          ie: '11',
          edge: '12',
          chrome: '21',
          firefox: '28',
          safari: '7'
        },
        modules: 'commonjs',
        useBuiltIns: 'usage',
        corejs: 3
      }
    ]
  ],
  plugins: [
    '@babel/plugin-transform-runtime',
    '@babel/plugin-transform-class-properties',
    '@babel/plugin-transform-async-to-generator'
  ]
};