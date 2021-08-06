import * as React from 'react'

// import * as styles from './TestComponent.module.scss'

// Mocking the above CSS Module `styles` object
const styles = {
  class1: '',
  class2: ''
}

type Props {
  conditionalValue1: boolean
  conditionalValue2?: boolean
}

export const TestComponent = (props: Props) => {
  const { conditionalValue } = props

  return (
    <li>
      {conditionalValue ? (
        <span
          className={`some-other-css-class ${styles.class1}`}
        >
         good morning
        </span>
      ) : (
        <div
          className={`another-css-class ${styles.class2}`}
        >
          good night
        </div>
      )}

      {conditionalValue2 !== undefined && (
        <div>
          incorrect syntax highlighting
        </div>
      )}
    </li>
  )
}