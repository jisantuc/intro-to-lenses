const R = require('ramda');

let someTeams = [
    {
        name: 'Civic Apps',
        employees: [
            {
                name: 'Alice',
                skills: [
                    {
                        name: 'possum identification',
                        isCool: false
                    },
                    {
                        name: 'terrifying martial arts',
                        isCool: true
                    }
                ]
            },
            {
                name: 'Arianna',
                skills: [
                    {
                        name: 'making cents',
                        isCool: false
                    },
                    {
                        name: 'frisbee',
                        isCool: true
                    }
                ]
            }
        ]
    },
    {
        name: 'Inchoate Team',
        employees: []
    }
];

// helpers because I'm lazy
let ix = R.lensIndex;
let prop = R.lensProp;

// Focus on whether the first team's first employee's first skill is cool
let focusFirstSkill = R.compose(ix(0),
                                    prop('employees'),
                                    ix(0),
                                    prop('skills'),
                                    ix(0),
                                    prop('isCool'));

// Try to get the 99th team member's special skills from the second team
let bogusLens = R.compose(ix(1),
                          R.lensProp('employees'),
                          ix(99),
                          R.lensProp('skills'));

// Result of applying `set` to the focus from focusFirstSkill
let withCoolFirstSkill = R.set(focusFirstSkill, true, someTeams);

module.exports = {
    ix,
    prop,
    bogusLens,
    someTeams,
    withCoolFirstSkill,
    focusFirstSkill
};
