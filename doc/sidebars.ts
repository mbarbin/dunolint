import type { SidebarsConfig } from '@docusaurus/plugin-content-docs';

/**
 * Creating a sidebar enables you to:
 - create an ordered group of docs
 - render a sidebar for each doc of that group
 - provide next/previous navigation

 The sidebars can be generated from the filesystem, or explicitly defined here.

 Create as many sidebars as you want.
 */
const sidebars: SidebarsConfig = {

  tutorialsSidebar: [
    {
      type: 'category',
      label: 'Tutorials',
      items: [
        { type: 'doc', id: 'tutorials/quick-start/README', label: 'Quick Start' },
        { type: 'doc', id: 'tutorials/first-run-through/README', label: 'First Run Through' },
      ],
    },
  ],

  guidesSidebar: [
    {
      type: 'category',
      label: 'Guides',
      items: [
        { type: 'doc', id: 'guides/installation', label: 'Installation' },
        { type: 'doc', id: 'guides/reformatter', label: 'Using dunolint as an Emacs reformatter' },
        { type: 'doc', id: 'guides/pre-commit', label: 'Using dunolint in pre-commit hooks' },
        {
          type: 'category', label: 'Migration Guides',
          link: { type: 'doc', id: 'guides/migration-guides/README' },
          items: [
            { type: 'doc', id: 'guides/migration-guides/is-prefix-is-suffix-absent-fields', label: 'is_prefix/is_suffix on absent fields' },
          ]
        },
      ],
    },
  ],

  referenceSidebar: [
    {
      type: 'category',
      label: 'Reference',
      items: [
        { type: 'doc', id: 'reference/odoc', label: 'OCaml Packages' },
        { type: 'doc', id: 'reference/default-rules', label: 'Default Rules' },
        {
          type: 'category', label: 'Config Language Reference',
          link: { type: 'doc', id: 'reference/config/README' },
          items: [
            { type: 'doc', id: 'reference/config/dune', label: 'dune' },
            { type: 'doc', id: 'reference/config/dune-project', label: 'dune-project' },
            { type: 'doc', id: 'reference/config/dune-workspace', label: 'dune-workspace' },
            { type: 'doc', id: 'reference/config/dunolint', label: 'dunolint' },
          ]
        },
      ],
    },
  ],

  explanationSidebar: [
    {
      type: 'category',
      label: 'Explanation',
      items: [
        { type: 'doc', id: 'explanation/README', label: 'Introduction' },
        { type: 'doc', id: 'explanation/current-state/README', label: 'Current State' },
        { type: 'doc', id: 'explanation/workspace-root', label: 'Workspace Root' },
        {
          type: 'category', label: 'Config',
          items: [
            { type: 'doc', id: 'explanation/config/language-design', label: 'Config Language Design' },
            { type: 'doc', id: 'explanation/config/autoloading', label: 'Config Autoloading' },
          ]
        },
        { type: 'doc', id: 'explanation/canonical-ordering', label: 'Canonical Ordering' },
        {
          type: 'category', label: 'Linting Equilibrium',
          link: { type: 'doc', id: 'explanation/linting-equilibrium/README' },
          items: [
            { type: 'doc', id: 'explanation/linting-equilibrium/comments-in-libraries', label: 'Comments in libraries' },
          ]
        },
      ],
    },
  ],
};

export default sidebars;
