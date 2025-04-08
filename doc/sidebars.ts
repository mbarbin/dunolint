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
        { type: 'doc', id: 'tutorials/installation/README', label: 'Installation' },
      ],
    },
  ],

  guidesSidebar: [
    {
      type: 'category',
      label: 'Guides',
      items: [
        { type: 'doc', id: 'guides/README', label: 'Introduction' },
        { type: 'doc', id: 'guides/reformatter', label: 'Using dunolint as an Emacs reformatter' },
      ],
    },
  ],

  referenceSidebar: [
    {
      type: 'category',
      label: 'Reference',
      items: [
        { type: 'doc', id: 'reference/odoc', label: 'OCaml Packages' },
        {
          type: 'category', label: 'Config Language Reference',
          link: { type: 'doc', id: 'reference/config/README' },
          items: [
            { type: 'doc', id: 'reference/config/dune', label: 'dune' },
            { type: 'doc', id: 'reference/config/dune-project', label: 'dune-project' },
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
        { type: 'doc', id: 'explanation/config/README', label: 'Config Language Design' },
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
